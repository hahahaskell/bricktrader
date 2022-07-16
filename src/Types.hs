module Types where

type Second = Int
type Millisecond = Int
type MicroSecond = Int
type Unit = String

oneSecond :: MicroSecond
oneSecond = 1000000


data BinanceOptions = BinanceOptions
  { apiKey :: String
  , apiSecret :: String
  }


