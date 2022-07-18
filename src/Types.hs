module Types where

import Data.ByteString (ByteString)

type Second = Int
type Millisecond = Int
type MicroSecond = Int
type Unit = String

type EpochTime = Integer
type Latency = Millisecond

type Url = String
type Body = ByteString

type WeightCount = Int
type ApiSecret = String
type ApiKey = String
type RetryAttempt = Int
type FailureMessage = String


oneSecond :: MicroSecond
oneSecond = 1000000

data BinanceOptions = BinanceOptions
  { apiKey :: String
  , apiSecret :: String
  }

-- may remove this
-- data BrickTraderConfig = BrickTraderConfig
--         { apiKey :: String
--         , apiSecret :: String
--         , symbols :: [Text]
--         } deriving (Show, Generic)

-- instance FromJSON BrickTraderConfig where
--     parseJSON = withObject "Config" $ \c ->
--             BrickTraderConfig <$> c .: "apiKey"
--                               <*> c .: "apiSecret"
--                               <*> c .: "symbols"

