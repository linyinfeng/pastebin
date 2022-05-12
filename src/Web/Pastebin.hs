{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Web.Pastebin
  ( pastebin,
    pastebin',
    pastebinIO,
    PastebinEnv (..),
    PastebinT,
    PastebinError,
  )
where

import qualified Amazonka as AWS
import qualified Amazonka.Data.Time as Time
import qualified Amazonka.S3 as S3
import Amazonka.S3.Lens
import Control.Lens
import Control.Lens.TH ()
import Control.Monad.Catch
import Control.Monad.Random
import Control.Monad.Reader
import qualified Control.Monad.Trans.Random as TR
import Control.Monad.Trans.Resource
import Data.Binary.Builder (fromByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Either.Combinators (rightToMaybe)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import Magic.Operations
import Magic.Types
import NeatInterpolation (trimming)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Parse
import Web.Pastebin.Option

data PastebinEnv = PastebinEnv
  { _pbOpts :: PastebinOptions,
    _awsEnv :: AWS.Env,
    _magic :: Magic
  }

makeLenses ''PastebinEnv

data PastebinError
  = ErrorNotFound
  | ErrorInvalidBodyType
  | ErrorInvalidBody
  | ErrorInvalidAWSGetObjectResponse S3.GetObjectResponse
  deriving (Show)

instance Exception PastebinError

newtype PastebinT g m a = PastebinT {unPastebinT :: ReaderT PastebinEnv (RandT g (ResourceT m)) a}
  deriving (Functor, Applicative, Monad, MonadRandom, MonadIO, MonadReader PastebinEnv)

-- , MonadResource, MonadThrow, MonadCatch
runPastebinT :: (RandomGen g, MonadUnliftIO m) => PastebinEnv -> g -> PastebinT g m a -> m a
runPastebinT env gen = runResourceT . flip evalRandT gen . flip runReaderT env . unPastebinT

instance MonadTrans (PastebinT g) where
  lift = PastebinT . lift . lift . lift

instance (MonadIO m) => MonadResource (PastebinT g m) where
  liftResourceT = PastebinT . lift . lift . liftResourceT

instance (MonadThrow m) => MonadThrow (PastebinT g m) where
  throwM = lift . throwM

instance (MonadCatch m) => MonadCatch (PastebinT g m) where
  catch m f = PastebinT (ReaderT (\r -> unReaderT (unPastebinT m) r `catchRand` (flip runReaderT r . unPastebinT . f)))
    where
      catchRand = TR.liftCatch catch
      unReaderT (ReaderT r) = r

pastebinIO :: PastebinEnv -> Application
pastebinIO env req respond = do
  g <- initStdGen
  runPastebinT env g (pastebin req respond)

pastebin :: (MonadRandom m, MonadCatch m, MonadIO m, MonadReader PastebinEnv m, MonadResource m) => Request -> (Response -> IO ResponseReceived) -> m ResponseReceived
pastebin req respond = catchIf (const True) (pastebin' req respond) (errorHandler req)
  where
    errorHandler r e = do
      response <- errorResponse r e
      liftIO (respond response)

errorResponse :: (MonadThrow m) => Request -> PastebinError -> m Response
errorResponse _req ErrorNotFound = return (responseBuilder notFound404 [plainContentType] "not found\n")
errorResponse _req ErrorInvalidBodyType = return (responseLBS badRequest400 [plainContentType] "bad request: require multipart/form-data\n")
errorResponse _req ErrorInvalidBody = return (responseLBS badRequest400 [plainContentType] "bad request: require one and only one file parameter\n")
errorResponse _req other = throwM other

plainContentType :: Header
plainContentType = ("Content-Type", "text/plain; charset=utf-8")

pastebin' :: (MonadRandom m, MonadCatch m, MonadIO m, MonadReader PastebinEnv m, MonadResource m) => Request -> (Response -> IO ResponseReceived) -> m ResponseReceived
pastebin' req respond =
  case (parseMethod (requestMethod req), pathInfo req) of
    (Right GET, []) -> getHelp req respond
    (Right GET, [key]) -> getObject req respond key
    (Right POST, []) -> postObject req respond
    (Right PUT, [key]) -> putObject req respond key
    (Right DELETE, [key]) -> deleteObject req respond key
    _ -> throwM ErrorNotFound

getHelp :: (MonadReader PastebinEnv m, MonadIO m) => Request -> (Response -> IO ResponseReceived) -> m ResponseReceived
getHelp req respond = do
  opts <- asks (^. pbOpts)
  let reqHeaders = M.fromList (requestHeaders req)
      url = serviceUrl opts reqHeaders
      response = responseLBS ok200 [plainContentType] (BL.fromStrict (encodeUtf8 (helpText url <> "\n")))
  liftIO (respond response)

helpText :: T.Text -> T.Text
helpText url =
  [trimming|
    # get help text
    curl $url

    # get an object
    curl $url/key

    # post an object
    curl -F "c=@filename" $url

    # post an object from stdin
    cat filename | curl -F "c=@-" $url

    # post an object with explicit content type
    curl -F "c=@filename;type=text/plain; charset=utf-8" $url

    # put an object
    curl -X PUT -F "c=@filename" $url/key

    # delete an object
    curl -X DELETE $url/key
  |]

getObject :: (MonadReader PastebinEnv m, MonadCatch m, MonadResource m, MonadIO m) => Request -> (Response -> IO ResponseReceived) -> T.Text -> m ResponseReceived
getObject req respond key = do
  bucket <- asks (^. pbOpts . optBucket)
  env <- asks (^. awsEnv)
  let reqHeaders = M.fromList (requestHeaders req)
      ifNoneMatch = fmap decodeUtf8 (M.lookup "If-None-Match" reqHeaders)
      ifModifiedSince = (M.lookup "If-ModifiedSince" reqHeaders >>= rightToMaybe . AWS.fromText . decodeUtf8) :: Maybe Time.ISO8601
      s3Req =
        S3.newGetObject (S3.BucketName bucket) (S3.ObjectKey (bucket <> "/" <> key))
          & getObject_ifNoneMatch .~ ifNoneMatch
          & getObject_ifModifiedSince .~ fmap (^. Time._Time) ifModifiedSince
  res' <- AWS.trying AWS._ServiceError $ AWS.send env s3Req
  case res' of
    Left err -> if err ^. AWS.serviceStatus == notFound404 then throwM ErrorNotFound else throwM (AWS.ServiceError err)
    Right res -> do
      resStatus <- case res ^. getObjectResponse_httpStatus of
        200 -> return ok200
        304 -> return notModified304
        _ -> throwM (ErrorInvalidAWSGetObjectResponse res)
      let resBody = res ^. getObjectResponse_body
          streamBody = bodyToStream resBody
          response = responseStream resStatus (headersFromAWSResponse res) streamBody
      liftIO (respond response)

headersFromAWSResponse :: S3.GetObjectResponse -> [Header]
headersFromAWSResponse res =
  catMaybes
    [ Just ("Content-Type", contentType),
      fmap ("Last-Modified",) lastModified,
      fmap ("ETag",) eTag,
      fmap ("Content-Length",) contentLength
    ]
  where
    contentType = encodeUtf8 (TL.toStrict (maybe defaultContentTypeLazy TL.fromStrict (res ^. getObjectResponse_contentType)))
    lastModified = fmap AWS.toBS (res ^. getObjectResponse_lastModified)
    eTag = fmap AWS.toBS (res ^. getObjectResponse_eTag)
    contentLength = fmap AWS.toBS (res ^. getObjectResponse_contentLength)

postObject :: (MonadReader PastebinEnv m, MonadRandom m, MonadCatch m, MonadResource m, MonadIO m) => Request -> (Response -> IO ResponseReceived) -> m ResponseReceived
postObject req respond = do
  opts <- asks (^. pbOpts)
  key <- freshKey
  putObject' req key
  let reqHeaders = M.fromList (requestHeaders req)
      resultUrl = createdUrl opts reqHeaders key
      response = responseLBS ok200 [plainContentType] (BL.fromStrict (encodeUtf8 (resultUrl <> "\n")))
  liftIO (respond response)

putObject :: (MonadReader PastebinEnv m, MonadThrow m, MonadResource m, MonadIO m) => Request -> (Response -> IO ResponseReceived) -> T.Text -> m ResponseReceived
putObject req respond key = do
  putObject' req key
  liftIO (respond (responseBuilder ok200 [] mempty))

putObject' :: (MonadReader PastebinEnv m, MonadThrow m, MonadResource m, MonadIO m) => Request -> T.Text -> m ()
putObject' req key = do
  bucket <- asks (^. pbOpts . optBucket)
  case getRequestBodyType req of
    Just (Multipart _) -> do
      (releaseKey, internalState) <- allocate createInternalState closeInternalState
      let backEnd = tempFileBackEnd internalState
      (bodyParams, bodyFiles) <- liftIO $ parseRequestBodyEx defaultParseRequestBodyOptions backEnd req
      when (not (null bodyParams) || length bodyFiles /= 1) (throwM ErrorInvalidBody)
      let (_, FileInfo _ providedContentType' filePath) = head bodyFiles
          providedContentType = decodeUtf8 providedContentType'
      contentType <-
        if providedContentType == defaultContentType
          then getContentTypeFromMagic filePath
          else return providedContentType
      s3ReqBody <- AWS.chunkedFile AWS.defaultChunkSize filePath
      let s3ReqBasic = S3.newPutObject (S3.BucketName bucket) (S3.ObjectKey (bucket <> "/" <> key)) s3ReqBody
          s3Req = s3ReqBasic & putObject_contentType ?~ contentType
      env <- asks (^. awsEnv)
      void $ AWS.send env s3Req
      release releaseKey
    _ -> throwM ErrorInvalidBody

getContentTypeFromMagic :: (MonadReader PastebinEnv m, MonadIO m) => FilePath -> m T.Text
getContentTypeFromMagic file = do
  magicInst <- asks (^. magic)
  result <- liftIO $ magicFile magicInst file
  return (T.pack result)

deleteObject :: (MonadReader PastebinEnv m, MonadResource m, MonadIO m) => Request -> (Response -> IO ResponseReceived) -> T.Text -> m ResponseReceived
deleteObject _req respond key = do
  bucket <- asks (^. pbOpts . optBucket)
  env <- asks (^. awsEnv)
  void $ AWS.send env (S3.newDeleteObject (S3.BucketName bucket) (S3.ObjectKey (bucket <> "/" <> key)))
  liftIO (respond (responseBuilder ok200 [] mempty))

serviceUrl :: PastebinOptions -> M.Map HeaderName B.ByteString -> T.Text
serviceUrl opts reqHeaders = case M.lookup "X-Forwarded-Host" reqHeaders of
  Just host ->
    let protocol = maybe "https" decodeUtf8 (M.lookup "X-Forwarded-Proto" reqHeaders)
        portStr = maybe "" ((":" <>) . decodeUtf8) (M.lookup "X-Forwarded-Port" reqHeaders)
     in (protocol <> "://" <> decodeUtf8 host <> portStr)
  Nothing -> "http://localhost:" <> fromString (show (opts ^. optPort))

createdUrl :: PastebinOptions -> M.Map HeaderName B.ByteString -> T.Text -> T.Text
createdUrl opts reqHeaders key = serviceUrl opts reqHeaders <> "/" <> key

defaultContentType :: T.Text
defaultContentType = "application/octet-stream"

defaultContentTypeLazy :: TL.Text
defaultContentTypeLazy = TL.fromStrict defaultContentType

bodyToStream :: AWS.ResponseBody -> StreamingBody
bodyToStream b send flush = AWS.sinkBody b (CC.map fromByteString .| CC.mapM_ (liftIO . send) >> liftIO flush)

freshKey :: (MonadReader PastebinEnv m, MonadRandom m, MonadCatch m, MonadResource m, MonadIO m) => m T.Text
freshKey = do
  initialNameLen <- asks (^. pbOpts . optShortestNameLength)
  findAvailableKey initialNameLen

findAvailableKey :: (MonadReader PastebinEnv m, MonadRandom m, MonadCatch m, MonadResource m, MonadIO m) => Int -> m T.Text
findAvailableKey len = do
  bucket <- asks (^. pbOpts . optBucket)
  env <- asks (^. awsEnv)
  candidate <- randomName len
  res <- AWS.trying AWS._ServiceError $ AWS.send env (S3.newHeadObject (S3.BucketName bucket) (S3.ObjectKey (bucket <> "/" <> candidate)))
  case res of
    Left err ->
      if err ^. AWS.serviceStatus == notFound404
        then return candidate
        else throwM (AWS.ServiceError err)
    Right _ -> findAvailableKey (len + 1)

randomName :: (MonadRandom m) => Int -> m T.Text
randomName len = fmap T.pack (replicateM len randomChar)

randomChar :: (MonadRandom m) => m Char
randomChar = do
  upperCase <- getRandom
  let range = if upperCase then ('A', 'Z') else ('a', 'z')
  getRandomR range
