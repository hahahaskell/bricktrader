{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Binance where

import Data.Text (Text)
import Data.Char
import qualified Data.Text as T
import Data.Map as Map (Map)

import Network.Wreq

import Control.Lens
import Data.Aeson (Value, toJSON, FromJSON (parseJSON), Options (fieldLabelModifier), genericParseJSON, defaultOptions, withObject, (.:))
import Data.Aeson.Lens (key, _String)
import Data.Aeson.Casing ( camelCase, aesonPrefix )
import qualified Control.Exception as E
import GHC.Generics (Generic)

-- {
--     "symbol": "LTCBTC",
--     "price": "0.00637700"
-- }
price :: String -> IO Text
price symbol = do
    r <- get $ priceUrl ++ "?symbol=" ++ symbol
    return $ r ^. (responseBody  . key "price" . _String)
    where
        priceUrl = "https://api.binance.com/api/v3/ticker/price"

--- [{
--     "symbol": "ETHBTC",
--     "price": "0.07749700"
-- },
-- {
--     "symbol": "LTCBTC",
--     "price": "0.00637700"
-- }]
data PriceResponse = PriceResponse
    { priceResponseSymbol :: Text
    , priceResponsePrice  :: Text
    }
    deriving Show

instance FromJSON PriceResponse where
    parseJSON = withObject "PriceResponse" $ \o ->
            PriceResponse <$> o .: "symbol"
                          <*> o .: "price"

prices :: [Text] ->  IO [PriceResponse]
prices symbols = do
    r <- asJSON =<< get priceUrl
    return $ filter match (r ^. responseBody)
    where
        priceUrl = "https://api.binance.com/api/v3/ticker/price"
        match a = priceResponseSymbol a `elem` symbols

--  [{
--         "symbol": "BTCAUD",
--         "priceChange": "-944.57000000",
--         "priceChangePercent": "-1.605",
--         "weightedAvgPrice": "57554.89400691",
--         "prevClosePrice": "58840.00000000",
--         "lastPrice": "57898.59000000",
--         "lastQty": "0.00099900",
--         "bidPrice": "57886.71000000",
--         "bidQty": "0.01094400",
--         "askPrice": "57938.81000000",
--         "askQty": "0.01249400",
--         "openPrice": "58843.16000000",
--         "highPrice": "59497.77000000",
--         "lowPrice": "54552.89000000",
--         "volume": "500.60052200",
--         "quoteVolume": "28812009.98351258",
--         "openTime": 1621246353952,
--         "closeTime": 1621332753952,
--         "firstId": 4163336,
--         "lastId": 4179963,
--         "count": 16628
--     }]

data TickerResponse = TickerResponse
    { tickerresponseSymbol             :: Text
    , tickerresponsePriceChange        :: Text
    , tickerresponsePriceChangePercent :: Text
    , tickerresponseWeightedAvgPrice   :: Text
    , tickerresponsePrevClosePrice     :: Text
    , tickerresponseLastPrice          :: Text
    , tickerresponseLastQty            :: Text
    , tickerresponseBidPrice           :: Text
    , tickerresponseBidQty             :: Text
    , tickerresponseAskPrice           :: Text
    , tickerresponseAskQty             :: Text
    , tickerresponseOpenPrice          :: Text
    , tickerresponseHighPrice          :: Text
    , tickerresponseLowPrice           :: Text
    , tickerresponseVolume             :: Text
    , tickerresponseQuoteVolume        :: Text
    , tickerresponseOpenTime           :: Int
    , tickerresponseCloseTime          :: Int
    , tickerresponseFirstId            :: Int
    , tickerresponseLastId             :: Int
    , tickerresponseCount              :: Int }
    deriving (Generic, Show)

instance FromJSON TickerResponse where
    parseJSON = genericParseJSON $ aesonPrefix camelCase

ticker :: IO [TickerResponse]
ticker = do
    r <- asJSON =<< get priceUrl
    return $ filter match (r ^. responseBody)
    where
        priceUrl = "https://api.binance.com/api/v3/ticker/24hr"
        symbols = ["BTCAUD", "ETHAUD", "XRPAUD", "BNBAUD", "DOGEAUD", "ADAAUD"]
        match a = tickerresponseSymbol a `elem` symbols


data StatusResponse = Status
    { statusresponseStatus :: Int
    , statusresponseMsg    :: Text
    }
    deriving (Generic, Show)

instance FromJSON StatusResponse where
    parseJSON = genericParseJSON $ aesonPrefix camelCase

getStatus :: IO StatusResponse
getStatus = do
    r <- asJSON =<< get statusUrl
    return $ r ^. responseBody
    where
        statusUrl = "https://api.binance.com/sapi/v1/system/status"
