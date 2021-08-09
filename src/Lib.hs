{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Lib where

import GHC.Generics
import Data.Aeson ( (.:), withObject, FromJSON(parseJSON) )
import Data.Text

type Second = Int
type Millisecond = Integer

data BrickTraderConfig = BrickTraderConfig
        { apiKey :: String
        , apiSecret :: String
        , symbols :: [Text]
        } deriving (Show, Generic)

instance FromJSON BrickTraderConfig where
    parseJSON = withObject "Config" $ \c ->
            BrickTraderConfig <$> c .: "apiKey"
                              <*> c .: "apiSecret"
                              <*> c .: "symbols"

data AppState = AppState
    { loggerContents :: [Text]
    , tickerContent :: String
    , binanceStatusContent :: String
    , weightCountContent :: String
    , binanceLoggerContents :: [Text]
    }