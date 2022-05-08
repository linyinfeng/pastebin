{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import qualified Amazonka as AWS
import Conduit
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Random
import Control.Monad.Trans.Random as R
import Data.Monoid
import Data.String
import qualified Options.Applicative as O
import qualified System.IO as IO
import Web.Pastebin
import Web.Pastebin.Option
import Web.Scotty.Trans

optionParser :: O.Parser PastebinOptions
optionParser =
  PastebinOptions
    <$> O.option O.str (O.long "endpoint-host" <> O.short 'e' <> O.metavar "HOSTNAME")
    <*> O.option O.auto (O.long "endpoint-port" <> O.metavar "PORT" <> O.value 443)
    <*> O.switch (O.long "endpoint-no-ssl")
    <*> O.option O.str (O.long "bucket" <> O.short 'b' <> O.metavar "NAME")
    <*> O.option O.auto (O.long "port" <> O.short 'p' <> O.metavar "PORT" <> O.value 3000)
    <*> O.option O.auto (O.long "shortest-name-length" <> O.short 's' <> O.metavar "NUM" <> O.value 6)

optionWithInfo :: O.ParserInfo PastebinOptions
optionWithInfo =
  O.info
    (optionParser O.<**> O.helper)
    O.fullDesc

main :: IO ()
main = do
  opts <- O.execParser optionWithInfo

  awsLogger <- AWS.newLogger AWS.Info IO.stdout
  awsDiscover <- AWS.newEnv AWS.discover
  let awsEnv =
        awsDiscover
          { AWS.envLogger = awsLogger,
            AWS.envOverride = serviceOverride opts
          }

  let env = PastebinEnv opts awsEnv
  let adapter :: RandT' StdGen IO a -> IO a
      adapter m = initStdGen >>= evalRandT' m
  runPastebinT (mapPastebinT (scottyT (opts ^. optPort) adapter) pastebin) env
  where
    serviceOverride opts =
      Dual $
        Endo $
          AWS.setEndpoint
            (not (opts ^. optEndpointNoSSL))
            (fromString (opts ^. optEndpointHost))
            (opts ^. optEndpointPort)

newtype RandT' g m a = RandT' {unRandT' :: RandT g m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadRandom)

instance MonadThrow m => MonadThrow (RandT' g m) where
  throwM = lift . throwM

instance MonadCatch m => MonadCatch (RandT' g m) where
  catch (RandT' r) f = RandT' $ R.liftCatch catch r (unRandT' . f)

evalRandT' :: Monad m => RandT' g m a -> g -> m a
evalRandT' (RandT' r) = evalRandT r
