{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import qualified Amazonka as AWS
import Control.Lens
import Data.Monoid
import Data.String
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.RequestLogger
import qualified Options.Applicative as O
import qualified System.IO as IO
import Text.Printf
import Web.Pastebin
import Web.Pastebin.Option

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
  let serviceOverride =
        Dual $
          Endo $
            AWS.setEndpoint
              (not (opts ^. optEndpointNoSSL))
              (fromString (opts ^. optEndpointHost))
              (opts ^. optEndpointPort)
      awsEnv =
        awsDiscover
          { AWS.envLogger = awsLogger,
            AWS.envOverride = serviceOverride
          }
      env = PastebinEnv opts awsEnv
      port = opts ^. optPort
      middlewares =
        [ logStdout,
          autohead
        ]
      app = foldr ($) (pastebinIO env) middlewares

  printf "listening on %d...\n" port
  Warp.run port app
