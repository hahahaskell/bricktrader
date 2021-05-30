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
import Data.Aeson.Casing ( camelCase )
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
    { tickerResponseSymbol             :: Text
    , tickerResponsePriceChange        :: Text
    , tickerResponsePriceChangePercent :: Text
    , tickerResponseWeightedAvgPrice   :: Text
    , tickerResponsePrevClosePrice     :: Text
    , tickerResponseLastPrice          :: Text
    , tickerResponseLastQty            :: Text
    , tickerResponseBidPrice           :: Text
    , tickerResponseBidQty             :: Text
    , tickerResponseAskPrice           :: Text
    , tickerResponseAskQty             :: Text
    , tickerResponseOpenPrice          :: Text
    , tickerResponseHighPrice          :: Text
    , tickerResponseLowPrice           :: Text
    , tickerResponseVolume             :: Text
    , tickerResponseQuoteVolume        :: Text
    , tickerResponseOpenTime           :: Int
    , tickerResponseCloseTime          :: Int
    , tickerResponseFirstId            :: Int
    , tickerResponseLastId             :: Int
    , tickerResponseCount              :: Int
    }
    deriving (Generic, Show)

instance FromJSON TickerResponse where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelCase . drop 14
        }

ticker :: IO [TickerResponse]
ticker = do
    r <- asJSON =<< get priceUrl
    return $ filter match (r ^. responseBody)
    where
        priceUrl = "https://api.binance.com/api/v3/ticker/24hr"
        symbols = ["BTCAUD", "ETHAUD", "XRPAUD", "BNBAUD", "DOGEAUD", "ADAAUD"]
        match a = tickerResponseSymbol a `elem` symbols

