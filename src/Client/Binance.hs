{-# LANGUAGE AllowAmbiguousTypes #-}
module Client.Binance
    ( createClient
    , Client
    , BinanceResult (Failure, Success)
    , SystemStatus (Offline, Online, Maintenance)
    , BinanceFailureResult (Halt, Sleep, Retry)
    , Ticker
    , WeightCount
    )
    where

import Import
import Network.Wreq
import qualified Network.Wreq.Session as Wreq
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as Http
import Data.Aeson (FromJSON (parseJSON), (.:))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Aeson.Lens (key, _String)
import Control.Lens ((^.), (&), (.~))
import Control.Concurrent.MVar (MVar, takeMVar, putMVar, newMVar)
import Control.Exception ( catch )
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import GHC.Generics (Generic)
import Data.Time ( defaultTimeLocale, getCurrentTime, formatTime )
import Data.Digest.Pure.SHA (hmacSha256)
import Util ( getWallTime )
import System.Clock (Clock(Monotonic), getTime)

type Latency = Millisecond
type Url = String
type Body = ByteString
type EpochTime = Integer
type WeightCount = Int
type ApiSecret = String
type ApiKey = String
type RetryAttempt = Int
type FailureMessage = String

data BinanceResult a b = Success a | Failure b deriving (Show)
data BinanceFailureResult =
        Halt String
        | Sleep Second
        | Retry String
    deriving (Show)

data Client
    = Client
    { healthCheck :: IO SystemStatus
    , systemTime :: IO (BinanceResult SystemTime BinanceFailureResult)
    , price :: Unit -> IO (BinanceResult Price BinanceFailureResult)
    , prices :: IO (BinanceResult [Price] BinanceFailureResult)
    , exchangeInfo :: IO (BinanceResult Exchangeinfo BinanceFailureResult)
    , ticker :: IO (BinanceResult [Ticker] BinanceFailureResult)
    , bookTicker :: IO (BinanceResult [Book] BinanceFailureResult)
    , order :: PlaceOrderOptions -> IO (BinanceResult Trade BinanceFailureResult)
    , openOrders :: String -> IO (BinanceResult [OpenOrders] BinanceFailureResult)
    , orderCount :: IO (BinanceResult [OrderLimit] BinanceFailureResult)
    , account :: IO (BinanceResult Account BinanceFailureResult)
    }

createClient :: BinanceOptions -> IO Client
createClient options = do
    wreqSession <- Wreq.newAPISession
    m <- newMVar BinanceHealth
            { latency = -1
            , system = Offline "Inactive"
            , time = -1
            , weight = -1
            }
    let health = BinanceHealthState m

    return Client
        { healthCheck = healthCheck_ health wreqSession
        , systemTime = binanceRequest health wreqSession (systemTime_ health)
        , price = binanceRequest health wreqSession . price_
        , prices = binanceRequest health wreqSession prices_
        , exchangeInfo = binanceRequest health wreqSession exchangeInfo_
        , ticker = binanceRequest health wreqSession ticker_
        , bookTicker = binanceRequest health wreqSession bookTicker_
        , order = binanceRequest health wreqSession . order_ options.apiKey options.apiSecret
        , openOrders = binanceRequest health wreqSession . openOrders_ options.apiKey options.apiSecret
        , orderCount = binanceRequest health wreqSession $ orderCount_ options.apiKey options.apiSecret
        , account = binanceRequest health wreqSession $ account_ options.apiKey options.apiSecret
        }

-- | Internal HTTP IO

class HasBinance a where
    httpGetRaw :: Wreq.Session -> Url -> IO (BinanceHttpResponse (Response LBS.ByteString))
    httpGet ::  FromJSON a => Wreq.Session -> Url -> IO (BinanceHttpResponse (Response a))
    httpGetWith ::  FromJSON a => Wreq.Session -> Network.Wreq.Options -> Url -> IO (BinanceHttpResponse (Response a))
    httpPostWith :: FromJSON a => Wreq.Session -> Network.Wreq.Options -> Body -> Url -> IO (BinanceHttpResponse (Response a))

instance HasBinance a where
    httpGetRaw session url = (Ok <$> Wreq.get session url) `catch` httpHandle
    httpGet session url = (Ok <$> (asJSON =<< Wreq.get session url)) `catch` httpHandle
    httpGetWith session opts url = (Ok <$> (asJSON =<< Wreq.getWith opts session url)) `catch` httpHandle
    httpPostWith session opts body url = (Ok <$> (asJSON =<< Wreq.postWith opts session url body)) `catch` httpHandle

newtype BinanceHealthState = BinanceHealthState (MVar BinanceHealth)
data BinanceHealth = BinanceHealth { latency :: Latency
                                   , system :: SystemStatus
                                   , time :: EpochTime
                                   , weight :: WeightCount
                                   } deriving (Show)

data BinanceHttpResponse a =
     Ok a
     | SecurityViolation
     | BackOff Second
     | IpBan Second
     | Fault String
     | ConnectionFailed String
     deriving (Show)

binanceRequest ::
    BinanceHealthState
    -> Wreq.Session
    -> (Wreq.Session -> IO (BinanceHttpResponse (Response a)))
    -> IO (BinanceResult a BinanceFailureResult)
binanceRequest (BinanceHealthState m) session request = do
    health <- takeMVar m
    putMVar m health

    if health.system == Online
    then do
        start <- getTime Monotonic
        response <- request session
        end <- getTime Monotonic

        newState <- takeMVar m
        putMVar m newState { latency = getWallTime start end }

        case response of
                Ok r -> do
                    h <- takeMVar m
                    putMVar m h { weight = getWeightCountHeader r }
                    return $ Success (r ^. responseBody)
                f -> return $ Failure $ mkFailure f
    else do
        return $ Failure $ Retry "Binance offline."
    where
        mkFailure :: BinanceHttpResponse a -> BinanceFailureResult
        mkFailure SecurityViolation = Halt "Binance's WAF has rejected the request."
        mkFailure (IpBan s) = Halt $ "IP banned for " <> show s
        mkFailure (Fault s) = Halt s
        mkFailure (BackOff s) = Sleep s
        mkFailure (ConnectionFailed s) = Retry s
        mkFailure _ = Halt "Unhandled failure"

-- Anything other than a 2XX throws an Exception. Handles documented binance expected status codes
httpHandle :: Http.HttpException -> IO (BinanceHttpResponse a)
httpHandle (Http.HttpExceptionRequest _ (Http.StatusCodeException res c)) =
    return $ case res ^. responseStatus . statusMessage of
                "FORBIDDEN"         -> SecurityViolation -- 403
                "TOO MANY REQUESTS" -> BackOff $ getRetryAfterHeader res -- 429
                "I'M A TEAPOT"      -> IpBan $ getRetryAfterHeader res -- 418
                "Bad Request"       -> Fault $ BS.unpack c -- 400
                s                   -> Fault $ BS.unpack s
httpHandle e = return . ConnectionFailed $ show e

-- | Headers Utils

getHeader :: Http.Response body -> Http.HeaderName -> String
getHeader res name = show $ res ^. responseHeader name

getRetryAfterHeader :: Http.Response () -> Int
getRetryAfterHeader r  = fromMaybe defaultRetryAfter $ readMaybe $ getHeader r "Retry-After"
        where defaultRetryAfter = 10 -- TODO move this

getWeightCountHeader :: Response body -> WeightCount
getWeightCountHeader r = fromMaybe defaultWeightCount $ readMaybe $ getHeader r "x-mbx-used-weight-1m"
        where defaultWeightCount = -1

-- | API Data and http calls

data SystemStatus =
    Online
    | Maintenance Text
    | Offline Text
    deriving (Show, Eq)

healthCheck_ :: BinanceHealthState -> Wreq.Session -> IO SystemStatus
healthCheck_ (BinanceHealthState m) session = do
    response <- httpGetRaw session "https://api.binance.com/sapi/v1/system/status"

    case response of
            Ok r -> case r ^. (responseBody . key "msg" . _String) of
                "normal" -> do
                    health <- takeMVar m
                    putMVar m health { system = Online }
                    return Online
                message  -> return $ Maintenance message
            f -> return $ Offline . Text.pack $ show f

data SystemTime = SystemTime
    { serverTime :: EpochTime
    } deriving (Generic, Show)

instance FromJSON SystemTime where
    parseJSON = Aeson.withObject "TimeResponse" $ \o ->
                    SystemTime <$> o .: "serverTime"

systemTime_ :: BinanceHealthState -> Wreq.Session -> IO (BinanceHttpResponse (Response SystemTime))
systemTime_ (BinanceHealthState m) session  = do
    response <- httpGet session "https://api.binance.com/api/v3/time"

    case response of
            Ok r -> do
                    health <- takeMVar m
                    putMVar m health { time = (r ^. responseBody).serverTime }
            _ -> pure ()

    return response

data Price = Price
    { symbol :: Text
    , price  :: Text
    }
    deriving (Show, Generic, FromJSON)

price_ :: String -> Wreq.Session -> IO (BinanceHttpResponse (Response Price))
price_ symbol session = httpGet session $ "https://api.binance.com/api/v3/ticker/price" ++ "?symbol=" ++ symbol

prices_ :: Wreq.Session -> IO (BinanceHttpResponse (Response [Price]))
prices_ session = httpGet session "https://api.binance.com/api/v3/ticker/price"

data Symbol = Symbol
    {   symbol :: Text
    ,   status :: Text
    ,   baseAsset :: Text
    ,   baseAssetPrecision :: Int
    ,   quoteAsset :: Text
    ,   baseCommissionPrecision :: Int
    ,   quoteCommissionPrecision :: Int
    ,   orderTypes :: [Text]
    ,   icebergAllowed :: Bool
    ,   ocoAllowed :: Bool
    ,   quoteOrderQtyMarketAllowed :: Bool
    ,   isSpotTradingAllowed :: Bool
    ,   isMarginTradingAllowed :: Bool
    ,   permissions :: [Text]
    } deriving (Generic, Show, FromJSON)

data Ratelimits = Ratelimits
    {   rateLimitType :: Text
    ,   interval :: Text
    ,   intervalNum :: Int
    ,   limit :: Int
    } deriving (Generic, Show, FromJSON)

data Exchangeinfo = Exchangeinfo
    {   timezone :: Text
    ,   serverTime :: Integer
    ,   rateLimits :: [Ratelimits]
    ,   symbols :: [Symbol]
    } deriving (Generic, Show, FromJSON)

exchangeInfo_ :: Wreq.Session -> IO (BinanceHttpResponse (Response Exchangeinfo))
exchangeInfo_ session = httpGet session "https://api.binance.com/api/v3/exchangeInfo"

data Ticker = Ticker
    { symbol             :: Text
    , priceChange        :: Text
    , priceChangePercent :: Text
    , weightedAvgPrice   :: Text
    , prevClosePrice     :: Text
    , lastPrice          :: Text
    , lastQty            :: Text
    , bidPrice           :: Text
    , bidQty             :: Text
    , askPrice           :: Text
    , askQty             :: Text
    , openPrice          :: Text
    , highPrice          :: Text
    , lowPrice           :: Text
    , volume             :: Text
    , quoteVolume        :: Text
    , openTime           :: Int
    , closeTime          :: Int
    , firstId            :: Int
    , lastId             :: Int
    , count              :: Int
    }
    deriving (Generic, Show, FromJSON)

ticker_ :: Wreq.Session -> IO (BinanceHttpResponse (Response [Ticker]))
ticker_ session = httpGet session "https://api.binance.com/api/v3/ticker/24hr"

data Book = Book { symbol :: Text
                 , bidPrice :: Text
                 , bidQty :: Text
                 , askPrice :: Text
                 , askQty :: Text
                 }
                 deriving (Generic, Show, FromJSON)

bookTicker_ :: Wreq.Session -> IO (BinanceHttpResponse (Response [Book]))
bookTicker_ session = httpGet session "https://api.binance.com/api/v3/ticker/bookTicker"

data Fill = Fill
     { fillPrice :: Text
     , fillQty :: Text
     , fillCommission :: Text
     , fillCommissionAsset :: Text
     } deriving (Generic, Show, FromJSON)

data Trade = Trade
    { symbol :: Text
    , orderId :: Int
    , orderListId :: Int
    , clientOrderId :: Text
    , transactTime :: Integer
    , price :: Text
    , origQty :: Text
    , executedQty :: Text
    , cummulativeQuoteQty :: Text
    , status :: Text
    , timeInForce :: Text
    -- , type :: Text
    , side :: Text
    , fills :: [Fill]
    } deriving (Generic, Show, FromJSON)

data PlaceOrderOptions = PlaceOrderOptions
    { symbol :: String
    , orderSide :: String
    , orderType :: String
    , timeInForce :: String
    , quantity :: String
    , price :: Double
    , recvWindow :: Int
    }

order_ :: ApiKey -> ApiSecret -> PlaceOrderOptions -> Wreq.Session -> IO (BinanceHttpResponse (Response Trade))
order_ apiKey apiSecret orderOptions session = do
    now <- getCurrentTime

    let timestamp = formatTime defaultTimeLocale "%s%3q" now
        body = LBS.pack $ "symbol=" <> orderOptions.symbol
                        <> "&side=" <> orderOptions.orderSide
                        <> "&type=" <> orderOptions.orderType
                        <> "&timeInForce=" <> orderOptions.timeInForce
                        <> "&quantity=" <> orderOptions.quantity
                        <> "&price=" <> show orderOptions.price
                        <> "&recvWindow=1000"
                        <> "&timestamp=" <> timestamp
        signature = Text.pack $ show $ hmacSha256 (LBS.pack apiSecret) body
        opts = defaults & header "X-MBX-APIKEY" .~ [BS.pack apiKey]
                        & param "signature" .~ [signature]

    httpPostWith session opts (LBS.toStrict body) "https://api.binance.com/api/v3/order"

data OpenOrders = OpenOrders
    { symbol :: String
    , orderId :: Int
    , orderListId :: Int -- Unless OCO, the value will always be -1
    , clientOrderId :: String
    , price :: String
    , origQty :: String
    , executedQty :: String
    , cummulativeQuoteQty :: String
    , status :: String
    , timeInForce :: String
    -- , type :: String
    , side :: String
    , stopPrice :: String
    , icebergQty :: String
    -- , time :: Integer
    , updateTime :: Integer
    , isWorking :: Bool
    , origQuoteOrderQty :: String
    } deriving (Generic, Show, FromJSON)

openOrders_ :: ApiKey -> ApiSecret  -> String -> Wreq.Session -> IO (BinanceHttpResponse (Response [OpenOrders]))
openOrders_ apiKey apiSecret  symbol session = do
    now <- getCurrentTime

    let timestamp = formatTime defaultTimeLocale "%s%3q" now
        queryString = LBS.pack $ "symbol=" <> symbol
                               <> "&recvWindow=10000"
                               <> "&timestamp=" <> timestamp

        signature = Text.pack $ show $ hmacSha256 (LBS.pack apiSecret) queryString
        opts = defaults & header "X-MBX-APIKEY" .~ [BS.pack apiKey]
                        & param "signature" .~ [signature]

    httpGetWith session opts ("https://api.binance.com/api/v3/openOrders" <> "?" <> LBS.unpack queryString)

data OrderLimit = OrderLimit
    { rateLimitType :: String
    , interval :: String
    , intervalNum :: Int
    , limit :: Int
    , count :: Int
    } deriving (Generic, Show, FromJSON)

orderCount_ :: ApiKey -> ApiSecret -> Wreq.Session -> IO (BinanceHttpResponse (Response [OrderLimit]))
orderCount_ apiKey apiSecret session = do
    now <- getCurrentTime

    let timestamp = formatTime defaultTimeLocale "%s%3q" now
        queryString = LBS.pack $ "recvWindow=10000"
                               <> "&timestamp=" <> timestamp
        signature = Text.pack $ show $ hmacSha256 (LBS.pack apiSecret) queryString
        opts = defaults & header "X-MBX-APIKEY" .~ [BS.pack apiKey]
                        & param "signature" .~ [signature]

    httpGetWith session opts ("https://api.binance.com/api/v3/rateLimit/order" <> "?" <> LBS.unpack queryString)

data Balance = Balance
    { asset :: String
    , free :: String
    , locked :: String
    } deriving (Generic, Show, FromJSON)

data Account = Account
    { makerCommission :: Int
    , takerCommission :: Int
    , buyerCommission :: Int
    , sellerCommission :: Int
    , canTrade :: Bool
    , canWithdraw :: Bool
    , canDeposit :: Bool
    , updateTime :: Integer
    , accountType :: String
    , balances :: [Balance]
    , permissions :: [String]
    } deriving (Generic, Show, FromJSON)

account_ :: ApiKey -> ApiSecret -> Wreq.Session -> IO (BinanceHttpResponse (Response Account))
account_ apiKey apiSecret session = do
    now <- getCurrentTime

    let timestamp = formatTime defaultTimeLocale "%s%3q" now
        queryString =  "recvWindow=10000"
                               <> "&timestamp=" <> timestamp
        signature = Text.pack $ show $ hmacSha256 (LBS.pack apiSecret) (LBS.pack queryString)
        opts = defaults & header "X-MBX-APIKEY" .~ [BS.pack apiKey]
                        & param "signature" .~ [signature]

    httpGetWith session opts ("https://api.binance.com/api/v3/account" <> "?" <> queryString)
