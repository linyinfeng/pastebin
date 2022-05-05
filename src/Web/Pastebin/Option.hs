{-# LANGUAGE TemplateHaskell #-}

module Web.Pastebin.Option
  ( PastebinOptions (..),
    optEndpointHost,
    optEndpointPort,
    optEndpointNoSSL,
    optBucket,
    optPort,
    optShortestNameLength,
  )
where

import Control.Lens
import Data.Text

data PastebinOptions = PastebinOptions
  { _optEndpointHost :: String,
    _optEndpointPort :: Int,
    _optEndpointNoSSL :: Bool,
    _optBucket :: Text,
    _optPort :: Int,
    _optShortestNameLength :: Int
  }
  deriving (Show)

makeLenses ''PastebinOptions
