{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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
import qualified Data.Map as M
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import NeatInterpolation (text)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Parse
import Web.Pastebin.Option

data PastebinEnv = PastebinEnv
  { _pbOpts :: PastebinOptions,
    _awsEnv :: AWS.Env
  }

makeLenses ''PastebinEnv

data PastebinError
  = ErrorNotFound
  | ErrorInvalidBodyType
  | ErrorInvalidBody
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

errorResponse :: (Monad m) => Request -> PastebinError -> m Response
errorResponse _req ErrorNotFound = return (responseBuilder notFound404 [plainContentType] "not found\n")
errorResponse _req ErrorInvalidBodyType = return (responseLBS badRequest400 [plainContentType] "bad request: require multipart/form-data\n")
errorResponse _req ErrorInvalidBody = return (responseLBS badRequest400 [plainContentType] "bad request: require one and only one file parameter\n")

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
      response = responseLBS ok200 [plainContentType] (BL.fromStrict (encodeUtf8 (helpText url)))
  liftIO (respond response)

helpText :: T.Text -> T.Text
helpText url =
  [text|
    # get help text
    curl $url

    # get an object
    curl $url/key

    # post an object
    curl -F "c=@filename" $url

    # post an object from stdin
    cat filename | curl -F "c=@-" $url

    # post an object with content type
    curl -F "c=@filename;type=text/plain" $url

    # put an object
    curl -X PUT -F "c=@filename" $url/key

    # delete an object
    curl -X DELETE $url/key
  |]

getObject :: (MonadReader PastebinEnv m, MonadCatch m, MonadResource m, MonadIO m) => Request -> (Response -> IO ResponseReceived) -> T.Text -> m ResponseReceived
getObject _req respond key = do
  bucket <- asks (^. pbOpts . optBucket)
  env <- asks (^. awsEnv)
  res' <- AWS.trying AWS._ServiceError $ AWS.send env (S3.newGetObject (S3.BucketName bucket) (S3.ObjectKey (bucket <> "/" <> key)))
  case res' of
    Left err -> if err ^. AWS.serviceStatus == notFound404 then throwM ErrorNotFound else throwM (AWS.ServiceError err)
    Right res -> do
      let contentType = encodeUtf8 (TL.toStrict (maybe defaultContentTypeLazy TL.fromStrict (res ^. getObjectResponse_contentType)))
          resBody = res ^. getObjectResponse_body
          streamBody = bodyToStream resBody
          response = responseStream ok200 [("Content-Type", contentType)] streamBody
      liftIO (respond response)

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
      let (_, FileInfo _ contentType filePath) = bodyFiles !! 0
      s3ReqBody <- AWS.chunkedFile AWS.defaultChunkSize filePath
      let s3ReqBasic = S3.newPutObject (S3.BucketName bucket) (S3.ObjectKey (bucket <> "/" <> key)) s3ReqBody
          s3Req = s3ReqBasic & putObject_contentType ?~ (decodeUtf8 contentType)
      env <- asks (^. awsEnv)
      void $ AWS.send env s3Req
      release releaseKey
    _ -> throwM ErrorInvalidBody

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
