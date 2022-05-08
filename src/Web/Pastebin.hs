{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Pastebin
  ( pastebin,
    PastebinT,
    mapPastebinT,
    runPastebinT,
    PastebinError (..),
    ScottyTE,
    PastebinEnv (..),
  )
where

import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import Amazonka.S3.Lens
import ClassyPrelude (fromByteVector)
import Conduit (PrimMonad, ResourceT, runResourceT)
import Control.Lens
import Control.Lens.TH ()
import Control.Monad.Catch
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Trans.Resource (MonadUnliftIO, transResourceT)
import Data.Binary.Builder (fromByteString)
import qualified Data.ByteString as B
import Data.Conduit
import Data.Conduit.Combinators (mapM_E, repeatWhileM, vectorBuilder)
import qualified Data.Conduit.Combinators as CC
import qualified Data.Map as M
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types (HeaderName)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai
import Web.Pastebin.Option
import Web.Scotty.Trans

data PastebinEnv = PastebinEnv
  { _pbOpts :: PastebinOptions,
    _awsEnv :: AWS.Env
  }

makeLenses ''PastebinEnv

type PastebinT m = ReaderT PastebinEnv (ResourceT m)

type ScottyTE = ScottyT PastebinError

type ActionTE = ActionT' PastebinError

pastebin :: (MonadRandom m, MonadCatch m, MonadIO m) => PastebinT (ScottyTE m) ()
pastebin = do
  mapPastebinT (get (capture "/:key") . unActionT') (getObject True)
  mapPastebinT (addroute HEAD (capture "/:key") . unActionT') (getObject False)
  mapPastebinT (post (literal "/") . unActionT') postObject
  mapPastebinT (put (capture "/:key") . unActionT') putObject
  mapPastebinT (delete (capture "/:key") . unActionT') deleteObject

mapPastebinT :: (m a -> n a) -> PastebinT m a -> PastebinT n a
mapPastebinT = mapReaderT . transResourceT

runPastebinT :: (MonadUnliftIO m) => PastebinT m a -> PastebinEnv -> m a
runPastebinT p env = runResourceT $ runReaderT p env

data PastebinError
  = ErrorPlain String
  | ErrorUnknownSize

instance ScottyError PastebinError where
  stringError = ErrorPlain
  showError (ErrorPlain e) = TL.pack e
  showError ErrorUnknownSize = "POST body size unknown"

newtype ActionT' e m a = ActionT' {unActionT' :: ActionT e m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadThrow, MonadCatch)

instance (MonadRandom m, ScottyError e) => MonadRandom (ActionT' e m) where
  getRandomR = lift . getRandomR
  getRandom = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms

liftAction :: (Monad m, ScottyError e) => ActionT e m a -> PastebinT (ActionT' e m) a
liftAction = lift . lift . ActionT'

getObject :: (MonadCatch m, MonadIO m, MonadRandom m) => Bool -> PastebinT (ActionTE m) ()
getObject hasBody = do
  bucket <- asks (^. pbOpts . optBucket)
  env <- asks (^. awsEnv)
  key <- liftAction $ param "key"
  res' <- AWS.trying AWS._ServiceError $ AWS.send env (S3.newGetObject (S3.BucketName bucket) (S3.ObjectKey (bucket <> "/" <> key)))
  case res' of
    Left err ->
      if err ^. AWS.serviceStatus == notFound404
        then liftAction $ status notFound404
        else throwM (AWS.ServiceError err)
    Right res -> do
      liftAction $ setHeader "Content-Type" (maybe defaultContentTypeLazy TL.fromStrict (res ^. getObjectResponse_contentType))
      let resBody = res ^. getObjectResponse_body
      let streamBody = bodyToStream resBody
      when hasBody $ liftAction $ stream streamBody

postObject :: (MonadCatch m, MonadRandom m, MonadIO m) => PastebinT (ActionTE m) ()
postObject = do
  opts <- asks (^. pbOpts)
  key <- freshKey
  putObject' key
  req <- liftAction request
  let reqHeaders = M.fromList (requestHeaders req)
  let resultUrl = createdUrl opts reqHeaders key
  liftAction $ text (TL.fromStrict resultUrl <> "\n")

putObject :: (MonadCatch m, MonadRandom m, MonadIO m) => PastebinT (ActionTE m) ()
putObject = do
  key <- liftAction $ param "key"
  putObject' key

putObject' :: (MonadCatch m, MonadRandom m, MonadIO m) => T.Text -> PastebinT (ActionTE m) ()
putObject' key = do
  bucket <- asks (^. pbOpts . optBucket)
  req <- liftAction request
  let reqHeaders = M.fromList (requestHeaders req)
  let bodyLength = requestBodyLength req
  case bodyLength of
    ChunkedBody -> liftAction $ raiseStatus badRequest400 ErrorUnknownSize
    KnownLength len' -> do
      let len = fromIntegral len'
      readBody <- liftAction bodyReader
      let s3BodyStream = readerToConduit readBody .| reChunk (fromIntegral AWS.defaultChunkSize)
      let s3ReqBody = AWS.unsafeChunkedBody AWS.defaultChunkSize len s3BodyStream
      let s3ReqBasic = S3.newPutObject (S3.BucketName bucket) (S3.ObjectKey (bucket <> "/" <> key)) s3ReqBody
      let contentType = maybe defaultContentType decodeUtf8 (M.lookup "Content-Type" reqHeaders)
      let s3Req = s3ReqBasic & putObject_contentType ?~ contentType
      env <- asks (^. awsEnv)
      void $ AWS.send env s3Req

deleteObject :: (MonadIO m) => PastebinT (ActionTE m) ()
deleteObject = do
  bucket <- asks (^. pbOpts . optBucket)
  env <- asks (^. awsEnv)
  key <- liftAction $ param "key"
  void $ AWS.send env (S3.newDeleteObject (S3.BucketName bucket) (S3.ObjectKey (bucket <> "/" <> key)))

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

readerToConduit :: MonadIO m => IO B.ByteString -> ConduitT i B.ByteString m ()
readerToConduit bsReader = repeatWhileM (liftIO bsReader) (not . B.null)

bodyToStream :: AWS.ResponseBody -> StreamingBody
bodyToStream b send flush = AWS.sinkBody b (CC.map fromByteString .| CC.mapM_ (liftIO . send) >> liftIO flush)

reChunk :: PrimMonad m => Int -> ConduitT B.ByteString B.ByteString m ()
reChunk size = vectorBuilder size mapM_E .| CC.map fromByteVector

freshKey :: (MonadCatch m, MonadRandom m, MonadIO m) => PastebinT (ActionTE m) T.Text
freshKey = do
  initialNameLen <- asks (^. pbOpts . optShortestNameLength)
  findAvailableKey initialNameLen

findAvailableKey :: (MonadCatch m, MonadRandom m, MonadIO m) => Int -> PastebinT (ActionTE m) T.Text
findAvailableKey len = do
  bucket <- asks (^. pbOpts . optBucket)
  env <- asks (^. awsEnv)
  candidate <- lift . lift $ randomName len
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
