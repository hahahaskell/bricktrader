{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Binance where

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LSC
import Data.Char()
import Data.Map()
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime, defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Digest.Pure.SHA (hmacSha256)
import qualified Control.Exception as E
import System.Clock (getTime, Clock (Monotonic), diffTimeSpec, TimeSpec (TimeSpec, nsec))

import Network.Wreq
import Network.Wreq.Types ( Options(headers), headers )
import Network.HTTP.Types.Header ( HeaderName )
import qualified Network.HTTP.Client as H
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.Wreq.Session as S
import Control.Lens ( (^.), (^?), (.~), (&) )
import Data.Aeson (FromJSON (parseJSON), genericParseJSON, withObject, (.:))
import Data.Aeson.Lens (key, _String, AsNumber (_Integer))
import Data.Aeson.Casing ( camelCase, aesonPrefix )
import Control.Concurrent.MVar ( MVar, newMVar, putMVar )
import Control.Concurrent (takeMVar)
import Lib

type FailureMessage = String
type Latency = Millisecond
type WeightCount = Int
type WeigthCountLimit = Int
type OrderCount = Int
type OrderCountLimit = Int
type Time = Integer

data BinanceResult a b = Success a | Failure b deriving (Show)
data BinanceHttpResponse a =
     Ok a
     | SecurityViolation
     | BackOff Second
     | IpBan Second
     | Fault String
     | ConnectionFailed String
     deriving (Show)

data BinanceFailureResult =
        Halt String
        | Sleep Second
        | Retry String
    deriving (Show)

mkSession :: IO S.Session
mkSession = S.newAPISession

getHeader :: Response body -> HeaderName -> String
getHeader res name = BSC.unpack $ res ^. responseHeader name

getWeightCountHeader :: Response body -> WeightCount
getWeightCountHeader res = read $ getHeader res "x-mbx-used-weight-1m"

getWeightOrderHeader :: Response body -> OrderCount
getWeightOrderHeader res = read $ getHeader res "x-mbx-order-count-1m"

getRetryAfter :: Response () -> Int
getRetryAfter r = read $ getHeader r "Retry-After" :: Int

httpHandle :: H.HttpException -> IO (BinanceHttpResponse a)
httpHandle (H.HttpExceptionRequest _ (H.StatusCodeException res c)) =
    return $ case res ^. responseStatus . statusMessage of
                "FORBIDDEN"         -> SecurityViolation -- 403
                "TOO MANY REQUESTS" -> BackOff $ getRetryAfter res -- 429
                "I'M A TEAPOT"      -> IpBan $ getRetryAfter res -- 418
                "Bad Request"       -> Fault $ BSC.unpack c -- 400
                s                   -> Fault $ BSC.unpack s
httpHandle e = return . ConnectionFailed $ show e

mkFailureResult :: BinanceHttpResponse a -> BinanceFailureResult
mkFailureResult SecurityViolation = Halt "Binance's WAF has rejected the request."
mkFailureResult (IpBan s) = Halt $ "IP banned for " <> show s
mkFailureResult (Fault s) = Halt s
mkFailureResult (BackOff s) = Sleep s
mkFailureResult (ConnectionFailed s) = Retry s
mkFailureResult _ = Halt "Unhandled failure"

data SystemStatus =
    Online
    | Maintenance String
    | Offline String
    deriving (Show, Eq)

systemStatus :: S.Session -> IO SystemStatus
systemStatus sess = do
    res <- (Ok <$> S.get sess statusUrl) `E.catch` httpHandle

    return $ case res of
        Ok r -> case r ^. (responseBody . key "msg" . _String) of
            "normal" -> Online
            m        -> Maintenance $ T.unpack m
        s -> Offline $ show s
    where
          statusUrl = "https://api.binance.com/sapi/v1/system/status"

systemTime :: S.Session -> IO (BinanceResult Millisecond BinanceFailureResult)
systemTime sess = do
    res <- (Ok <$> S.get sess timeUrl) `E.catch` httpHandle

    return $ case res of
        Ok r -> case r ^? responseBody . key "serverTime" . _Integer of
                Just t -> Success t
                Nothing -> Failure $ Halt "No time exists."
        f -> Failure $ mkFailureResult f

    where
        timeUrl = "https://api.binance.com/api/v3/time"

price :: S.Session -> String -> IO (BinanceResult (Text, WeightCount) BinanceFailureResult)
price sess symbol = do
    res <- (Ok <$> S.get sess priceUrl) `E.catch` httpHandle

    return $ case res of
            Ok r -> Success (r ^. (responseBody  . key "price" . _String), getWeightCountHeader r)
            f -> Failure $ mkFailureResult f
    where
        priceUrl = "https://api.binance.com/api/v3/ticker/price" ++ "?symbol=" ++ symbol

data Price = Price
    { priceSymbol :: Text
    , pricePrice  :: Text
    }
    deriving Show

instance FromJSON Price where
    parseJSON = withObject "PriceResponse" $ \o ->
            Price <$> o .: "symbol"
                  <*> o .: "price"

prices :: S.Session -> IO (BinanceResult ([Price], WeightCount) BinanceFailureResult)
prices sess = do
    res <- (Ok <$> (asJSON =<< S.get sess priceUrl)) `E.catch` httpHandle

    return $ case res of
            Ok r -> Success (r ^. responseBody, getWeightCountHeader r)
            f -> Failure $ mkFailureResult f
    where
        priceUrl = "https://api.binance.com/api/v3/ticker/price"

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
    , tickerCount              :: Int
    }
    deriving (Generic, Show)

instance FromJSON Ticker where
    parseJSON = genericParseJSON $ aesonPrefix camelCase

ticker :: S.Session -> IO (BinanceResult ([Ticker], WeightCount) BinanceFailureResult)
ticker sess = do
    res <- (Ok <$> (asJSON =<< S.get sess tickerUrl)) `E.catch` httpHandle

    return $ case res of
        Ok r -> Success (r ^. responseBody, getWeightCountHeader r)
        f -> Failure $ mkFailureResult f
    where
        tickerUrl = "https://api.binance.com/api/v3/ticker/24hr"

data Book = Book { bookSymbol :: Text
                 , bookBidPrice :: Text
                 , bookBidQty :: Text
                 , bookAskPrice :: Text
                 , bookAskQty :: Text
                 }
                 deriving (Generic, Show)

instance FromJSON Book where
    parseJSON = genericParseJSON $ aesonPrefix camelCase

booker :: S.Session -> IO (BinanceResult ([Book], WeightCount) BinanceFailureResult)
booker sess = do
    res <- (Ok <$> (asJSON =<< S.get sess bookUrl)) `E.catch` httpHandle

    return $ case res of
        Ok r -> Success (r ^. responseBody, getWeightCountHeader r)
        f    -> Failure $ mkFailureResult f
    where
         bookUrl = "https://api.binance.com/api/v3/ticker/bookTicker"

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

data Fill = Fill
     { fillPrice :: Text
     , fillQty :: Text
     , fillCommission :: Text
     , fillCommissionAsset :: Text
     } deriving (Generic, Show)

instance FromJSON Fill where
    parseJSON = genericParseJSON $ aesonPrefix camelCase

data Trade = Trade
    { tradeSymbol :: Text
    , tradeOrderId :: Int
    , tradeOrderListId :: Int
    , tradeClientOrderId :: Text
    , tradeTransactTime :: Integer
    , tradePrice :: Text
    , tradeOrigQty :: Text
    , tradeExecutedQty :: Text
    , tradeCummulativeQuoteQty :: Text
    , tradeStatus :: Text
    , tradeTimeInForce :: Text
    , tradeType :: Text
    , tradeSide :: Text
    , tradeFills :: [Fill]
    } deriving (Generic, Show)

instance FromJSON Trade where
    parseJSON = genericParseJSON $ aesonPrefix camelCase

placeOrder :: S.Session -> String -> String -> IO (BinanceResult (Trade, WeightCount, OrderCount) BinanceFailureResult)
placeOrder sess secret key = do
    now <- getCurrentTime

    let timestamp = formatTime defaultTimeLocale "%s%3q" now
        body = LSC.pack $ "symbol=DOTUSDT&side=BUY&type=LIMIT&timeInForce=FOK&quantity=1&price=17.400&recvWindow=1000&timestamp=" ++ timestamp
        signature = T.pack $ show $ hmacSha256 (LSC.pack secret) body
        opts = defaults & header "X-MBX-APIKEY" .~ [BSC.pack key]
                        & param "signature" .~ [signature]

    res <- (Ok <$> (asJSON =<< S.postWith opts sess orderUrl body)) `E.catch` httpHandle

    return $ case res of
        Ok r -> Success (r ^. responseBody, getWeightCountHeader r, getWeightOrderHeader r)
        f -> Failure $ mkFailureResult f
    where
        orderUrl = "https://api.binance.com/api/v3/order"

