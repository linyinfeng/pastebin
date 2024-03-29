{-# LANGUAGE TemplateHaskell #-}

module Web.Pastebin.Option
  ( PastebinOptions (..),
    AddressingStyle (..),
    optEndpointHost,
    optEndpointPort,
    optEndpointNoSSL,
    optBucket,
    optPort,
    optShortestNameLength,
    optAddressingStyle,
    unAddressingStyle,
  )
where

import qualified Amazonka as AWS
import Control.Lens
import Data.Text

newtype AddressingStyle = AddressingStyle {_unAddressingStyle :: AWS.S3AddressingStyle}
  deriving (Show)

data PastebinOptions = PastebinOptions
  { _optEndpointHost :: String,
    _optEndpointPort :: Int,
    _optEndpointNoSSL :: Bool,
    _optAddressingStyle :: AddressingStyle,
    _optBucket :: Text,
    _optPort :: Int,
    _optShortestNameLength :: Int
  }
  deriving (Show)

makeLenses ''AddressingStyle
makeLenses ''PastebinOptions

instance Read AddressingStyle where
  readsPrec _ "auto" = [(AddressingStyle AWS.S3AddressingStyleAuto, "")]
  readsPrec _ "path" = [(AddressingStyle AWS.S3AddressingStylePath, "")]
  readsPrec _ "virtual" = [(AddressingStyle AWS.S3AddressingStyleVirtual, "")]
  readsPrec _ _ = []
