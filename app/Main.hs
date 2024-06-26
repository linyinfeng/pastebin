{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import qualified Amazonka as AWS
import Control.Lens
import Data.String
import Magic.Init
import Magic.Types
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
    <*> O.option O.auto (O.long "addressing-style" <> O.metavar "STYLE" <> O.value (AddressingStyle AWS.S3AddressingStyleAuto))
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
        AWS.setEndpoint
          (not (opts ^. optEndpointNoSSL))
          (fromString (opts ^. optEndpointHost))
          (opts ^. optEndpointPort)
          . (AWS.service_s3AddressingStyle .~ (opts ^. (optAddressingStyle . unAddressingStyle)))
      awsEnv =
        awsDiscover
          { AWS.logger = awsLogger,
            AWS.overrides = serviceOverride
          }
  magicInst <- magicOpen [MagicMime] -- output both mime type and mime encoding
  magicLoadDefault magicInst
  let env = PastebinEnv opts awsEnv magicInst
      port = opts ^. optPort
      middlewares =
        [ logStdout,
          autohead
        ]
      app = foldr ($) (pastebinIO env) middlewares

  printf "listening on %d...\n" port
  Warp.run port app
