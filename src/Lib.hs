{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Lib where

import GHC.Generics
import Data.Aeson ( (.:), withObject, FromJSON(parseJSON) )

data BrickTraderConfig = BrickTraderConfig
        { apiKey :: String
        , secretKey :: String
        } deriving (Show, Generic)

instance FromJSON BrickTraderConfig where
    parseJSON = withObject "Config" $ \c ->
            BrickTraderConfig <$> c .: "apiKey"
                              <*> c .: "secretKey"
