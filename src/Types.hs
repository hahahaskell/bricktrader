module Types where

import Data.ByteString (ByteString)
import GHC.Generics
import Data.Aeson (FromJSON (..), withObject, (.:))

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

data Config = Config
  { apiKey :: String
  , apiSecret :: String
  } deriving (Show, Generic)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \c ->
                      Config <$> c .: "apiKey"
                             <*> c .: "apiSecret"

