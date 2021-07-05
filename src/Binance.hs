{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Binance where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char()
import Data.Map()
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

import Network.Wreq
import Network.HTTP.Types.Header
import qualified Network.HTTP.Client as N
import qualified Network.Wreq.Session as S

import Control.Lens ( (^.), (^?) )
import Data.Aeson (FromJSON (parseJSON), genericParseJSON, withObject, (.:))
import Data.Aeson.Lens (key, _String, AsNumber (_Integer))
import Data.Aeson.Casing ( camelCase, aesonPrefix )

import GHC.Generics (Generic)
import qualified Control.Exception as E
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

type Seconds = Int

data BinanceResult a b = Success a | Failure b deriving (Show)

-- https://binance-docs.github.io/apidocs/spot/en/#general-info
data BinanceHttpResponse a =
     Ok a
     | SecurityViolation
     | BackOff Seconds
     | IpBan Seconds
     | Fault String
     | ConnectionFailed String
     deriving (Show)

-- newtype RetryAfter = RetryAfter Seconds
newtype WeightCount = WeightCount Int
newtype OrderCount = OrderCount Int

instance Show WeightCount where
    show (WeightCount a) = show a
instance Show OrderCount where
    show (OrderCount a) = show a

-- data BinanceSuccessResult a = SuccessResponse WeightCount a
--     deriving (Show)

data BinanceFailureResult =
        Halt String
        | Sleep Seconds
        | Retry String
    deriving (Show)

mkSession :: IO S.Session
mkSession = S.newAPISession

mkAuthSession :: String -> String -> IO S.Session
mkAuthSession apikey p = S.newAPISession

getHeader :: Response body -> HeaderName -> String
getHeader res name = BSC.unpack $ res ^. responseHeader name

getWeightCountHeader :: Response body -> WeightCount
getWeightCountHeader res = WeightCount . read $ getHeader res "x-mbx-used-weight-1m"

httpHandle :: N.HttpException -> IO (BinanceHttpResponse a)
httpHandle (N.HttpExceptionRequest _ (N.StatusCodeException res _)) =
    return $ case res ^. responseStatus . statusMessage of
        "FORBIDDEN"         -> SecurityViolation -- 403
        "TOO MANY REQUESTS" -> BackOff $ getRetryAfter res -- 429
        "I'M A TEAPOT"      -> IpBan $ getRetryAfter res -- 418
        s                   -> Fault $ BSC.unpack s
  where
    getRetryAfter :: Response () -> Int
    -- getRetryAfter res = fromMaybe 0 (readMaybe (getHeader res "Retry-After") :: Maybe Int)
    getRetryAfter res = read $ getHeader res "Retry-After" :: Int
httpHandle e = return . ConnectionFailed $ show e

mkFailureResult :: BinanceHttpResponse a -> BinanceFailureResult
mkFailureResult SecurityViolation = Halt "Binance's WAF has rejected the request."
mkFailureResult (IpBan s) = Halt $ "IP banned for " <> show s
mkFailureResult (Fault s) = Halt s
mkFailureResult (BackOff s) = Sleep s
mkFailureResult (ConnectionFailed s) = Retry s

-- {
--     "symbol": "LTCBTC",
--     "price": "0.00637700"
-- }
price :: String -> IO (BinanceResult (Text, WeightCount) BinanceFailureResult)
price symbol = do
    res <- (Ok <$> get priceUrl) `E.catch` httpHandle

    return $ case res of
            Ok r -> Success (r ^. (responseBody  . key "price" . _String), getWeightCountHeader r)
            f -> Failure $ mkFailureResult f
    where
        priceUrl = "https://api.binance.com/api/v3/ticker/price" ++ "?symbol=" ++ symbol

--- [{
--     "symbol": "ETHBTC",
--     "price": "0.07749700"
-- },
-- {
--     "symbol": "LTCBTC",
--     "price": "0.00637700"
-- }]
data Price = Price
    { priceSymbol :: Text
    , pricePrice  :: Text
    }
    deriving Show

instance FromJSON Price where
    parseJSON = withObject "PriceResponse" $ \o ->
            Price <$> o .: "symbol"
                  <*> o .: "price"

prices :: IO (BinanceResult ([Price], WeightCount) BinanceFailureResult)
prices = do
    res <- (Ok <$> (asJSON =<< get priceUrl)) `E.catch` httpHandle
    return $ case res of
            Ok r -> Success (r ^. responseBody, getWeightCountHeader r)
            f -> Failure $ mkFailureResult f
    where
        priceUrl = "https://api.binance.com/api/v3/ticker/price"

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

data Ticker = Ticker
    { tickerSymbol             :: Text
    , tickerPriceChange        :: Text
    , tickerPriceChangePercent :: Text
    , tickerWeightedAvgPrice   :: Text
    , tickerPrevClosePrice     :: Text
    , tickerLastPrice          :: Text
    , tickerLastQty            :: Text
    , tickerBidPrice           :: Text
    , tickerBidQty             :: Text
    , tickerAskPrice           :: Text
    , tickerAskQty             :: Text
    , tickerOpenPrice          :: Text
    , tickerHighPrice          :: Text
    , tickerLowPrice           :: Text
    , tickerVolume             :: Text
    , tickerQuoteVolume        :: Text
    , tickerOpenTime           :: Int
    , tickerCloseTime          :: Int
    , tickerFirstId            :: Int
    , tickerLastId             :: Int
    , tickerCount              :: Int }
    deriving (Generic, Show)

instance FromJSON Ticker where
    parseJSON = genericParseJSON $ aesonPrefix camelCase

ticker :: S.Session -> IO (BinanceResult ([Ticker], WeightCount) BinanceFailureResult)
ticker sess = do
    res <- (Ok <$> (asJSON =<< S.get sess priceUrl)) `E.catch` httpHandle

    return $ case res of
        Ok r -> Success (r ^. responseBody, getWeightCountHeader r)
        f -> Failure $ mkFailureResult f
    where
        priceUrl = "https://api.binance.com/api/v3/ticker/24hr"

data SystemStatusResponse =
    Online
    | Maintenance String
    | Offline String
    deriving (Show)

systemStatus :: S.Session -> IO SystemStatusResponse
systemStatus sess = do
    res <- (Ok <$> S.get sess statusUrl) `E.catch` httpHandle

    return $ case res of
        Ok r -> case r ^. (responseBody . key "msg" . _String) of
            "normal" -> Online
            m        -> Maintenance $ T.unpack m
        s -> Offline $ show s
    where
        -- SAPI preceeds WAPI
          statusUrl = "https://api.binance.com/sapi/v1/system/status"

data Symbol = Symbol
    {   symbolSymbol :: Text
    ,   symbolStatus :: Text
    ,   symbolBaseAsset :: Text
    ,   symbolBaseAssetPrecision :: Int
    ,   symbolQuoteAsset :: Text
    ,   symbolBaseCommissionPrecision :: Int
    ,   symbolQuoteCommissionPrecision :: Int
    ,   symbolOrderTypes :: [Text]
    ,   symbolIcebergAllowed :: Bool
    ,   symbolOcoAllowed :: Bool
    ,   symbolQuoteOrderQtyMarketAllowed :: Bool
    ,   symbolIsSpotTradingAllowed :: Bool
    ,   symbolIsMarginTradingAllowed :: Bool
    ,   symbolPermissions :: [Text]
    } deriving (Generic, Show)

instance FromJSON Symbol where
    parseJSON = genericParseJSON $ aesonPrefix camelCase

data Ratelimits = Ratelimits
    {   ratelimitsRateLimitType :: Text
    ,   ratelimitsInterval :: Text
    ,   ratelimitsIntervalNum :: Int
    ,   ratelimitsLimit :: Int
    } deriving (Generic, Show)

instance FromJSON Ratelimits where
    parseJSON = genericParseJSON $ aesonPrefix camelCase

data Exchangeinfo = Exchangeinfo
    {   exchangeinfoTimezone :: Text
    ,   exchangeinfoServerTime :: Integer
    ,   exchangeinfoRateLimits :: [Ratelimits]
    ,   exchangeinfoSymbols :: [Symbol]
    } deriving (Generic, Show)

instance FromJSON Exchangeinfo where
    parseJSON = genericParseJSON $ aesonPrefix camelCase

exchangeInfo :: S.Session -> IO (BinanceResult Exchangeinfo BinanceFailureResult)
exchangeInfo sess = do
    res <- (Ok <$> (asJSON =<< S.get sess exchangeInfoUrl)) `E.catch` httpHandle

    return $ case res of
        Ok r -> Success (r ^. responseBody)
        f -> Failure $ mkFailureResult f
    where
        exchangeInfoUrl = "https://api.binance.com/api/v3/exchangeInfo"


