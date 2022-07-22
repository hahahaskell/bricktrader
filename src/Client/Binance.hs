module Client.Binance
    ( createClient
    , Client
    , BinanceOptions (..)
    , BinanceResult (..)
    , SystemStatus (..)
    , BinanceHttpResponse (..)
    , BinanceFailureResult (..)
    , Ticker
    , WeightCount
    , PlaceOrderOptions
    , Price
    , Exchangeinfo (..)
    , Symbol (..)
    , Book
    , Trade
    , OpenOrders
    , OrderLimit
    , Account
    , SystemTime
    )
    where

import Types
import Util
import Network.Wreq
import qualified Network.Wreq.Session as Wreq
import qualified Network.HTTP.Client as Http
import Data.Aeson (FromJSON (parseJSON), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Aeson.Lens (key, _String)
import Control.Lens ((^.), (&), (.~))
import Control.Exception ( catch )
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import GHC.Generics (Generic)
import Data.Time ( defaultTimeLocale, getCurrentTime, formatTime )
import Data.Digest.Pure.SHA (hmacSha256)

data BinanceResult a b = Success a | Failure b deriving (Show)
data BinanceFailureResult =
        Halt String
        | Sleep Second
        | Retry String
    deriving (Show)

data BinanceOptions = BinanceOptions
  { apiKey :: String
  , apiSecret :: String
  }

data Client
    = Client
    { healthCheck :: IO SystemStatus
    , systemTime :: IO (BinanceHttpResponse (Response SystemTime))
    , price :: Unit -> IO (BinanceHttpResponse (Response Price))
    , prices :: IO (BinanceHttpResponse (Response [Price]))
    , exchangeInfo :: IO (BinanceHttpResponse (Response Exchangeinfo))
    , ticker :: IO (BinanceHttpResponse (Response [Ticker]))
    , bookTicker :: IO (BinanceHttpResponse (Response [Book]) )
    , order :: PlaceOrderOptions -> IO (BinanceHttpResponse (Response Trade))
    , openOrders :: String -> IO (BinanceHttpResponse (Response [OpenOrders]))
    , orderCount :: IO (BinanceHttpResponse (Response [OrderLimit]))
    , account :: IO (BinanceHttpResponse (Response Account))
    }

createClient :: BinanceOptions -> IO Client
createClient options = do
    wreqSession <- Wreq.newAPISession

    return Client
        { healthCheck = healthCheck_ wreqSession
        , systemTime = systemTime_  wreqSession
        , price = price_ wreqSession
        , prices = prices_ wreqSession
        , exchangeInfo = exchangeInfo_ wreqSession
        , ticker = ticker_ wreqSession
        , bookTicker = bookTicker_ wreqSession
        , order = order_ wreqSession options.apiKey options.apiSecret
        , openOrders = openOrders_ wreqSession options.apiKey options.apiSecret
        , orderCount = orderCount_ wreqSession  options.apiKey options.apiSecret
        , account = account_ wreqSession options.apiKey options.apiSecret
        }

-- | Internal HTTP IO

class (Monad m) => HasBinance m where
    httpGetRaw :: Wreq.Session -> Url -> m (BinanceHttpResponse (Response LBS.ByteString))
    httpGet ::  FromJSON a => Wreq.Session -> Url -> m (BinanceHttpResponse (Response a))
    httpGetWith ::  FromJSON a => Wreq.Session -> Network.Wreq.Options -> Url -> m (BinanceHttpResponse (Response a))
    httpPostWith :: FromJSON a => Wreq.Session -> Network.Wreq.Options -> Body -> Url -> m (BinanceHttpResponse (Response a))

instance HasBinance IO where
    httpGetRaw session url = (Ok <$> Wreq.get session url) `catch` httpHandle
    httpGet session url = (Ok <$> (asJSON =<< Wreq.get session url)) `catch` httpHandle
    httpGetWith session opts url = (Ok <$> (asJSON =<< Wreq.getWith opts session url)) `catch` httpHandle
    httpPostWith session opts body url = (Ok <$> (asJSON =<< Wreq.postWith opts session url body)) `catch` httpHandle

data BinanceHttpResponse a =
     Ok a
     | SecurityViolation
     | BackOff Second
     | IpBan Second
     | Fault String
     | ConnectionFailed String
     deriving (Show)

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

getRetryAfterHeader :: Http.Response () -> Int
getRetryAfterHeader r  = fromMaybe defaultRetryAfter $ readMaybe $ getHeader r "Retry-After"
        where defaultRetryAfter = 10 -- TODO move this


-- | API Data and http calls

data SystemStatus =
    Online
    | Maintenance Text
    | Offline Text
    deriving (Show, Eq)

healthCheck_ :: (HasBinance m) => Wreq.Session -> m SystemStatus
healthCheck_ session = do
    response <- httpGetRaw session "https://api.binance.com/sapi/v1/system/status"

    case response of
            Ok r -> case r ^. (responseBody . key "msg" . _String) of
                "normal" -> return Online
                message  -> return $ Maintenance message
            f -> return $ Offline . Text.pack $ show f

data SystemTime = SystemTime
    { serverTime :: EpochTime
    } deriving (Generic, Show)

instance FromJSON SystemTime where
    parseJSON = Aeson.withObject "TimeResponse" $ \o ->
                    SystemTime <$> o .: "serverTime"

systemTime_ :: Wreq.Session -> IO (BinanceHttpResponse (Response SystemTime))
systemTime_ session  = httpGet session "https://api.binance.com/api/v3/time"

data Price = Price
    { symbol :: Text
    , price  :: Text
    }
    deriving (Show, Generic, FromJSON)

price_ :: Wreq.Session -> String -> IO (BinanceHttpResponse (Response Price))
price_ session symbol  = httpGet session $ "https://api.binance.com/api/v3/ticker/price" ++ "?symbol=" ++ symbol

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

order_ :: Wreq.Session -> ApiKey -> ApiSecret -> PlaceOrderOptions -> IO (BinanceHttpResponse (Response Trade))
order_ session key secret orderOptions = do
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
        signature = Text.pack $ show $ hmacSha256 (LBS.pack secret) body
        opts = defaults & header "X-MBX-APIKEY" .~ [BS.pack key]
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

openOrders_ :: Wreq.Session -> ApiKey -> ApiSecret  -> String -> IO (BinanceHttpResponse (Response [OpenOrders]))
openOrders_ session key secret symbol = do
    now <- getCurrentTime

    let timestamp = formatTime defaultTimeLocale "%s%3q" now
        queryString = LBS.pack $ "symbol=" <> symbol
                               <> "&recvWindow=10000"
                               <> "&timestamp=" <> timestamp

        signature = Text.pack $ show $ hmacSha256 (LBS.pack secret) queryString
        opts = defaults & header "X-MBX-APIKEY" .~ [BS.pack key]
                        & param "signature" .~ [signature]

    httpGetWith session opts ("https://api.binance.com/api/v3/openOrders" <> "?" <> LBS.unpack queryString)

data OrderLimit = OrderLimit
    { rateLimitType :: String
    , interval :: String
    , intervalNum :: Int
    , limit :: Int
    , count :: Int
    } deriving (Generic, Show, FromJSON)

orderCount_ :: Wreq.Session -> ApiKey -> ApiSecret -> IO (BinanceHttpResponse (Response [OrderLimit]))
orderCount_ session key secret = do
    now <- getCurrentTime

    let timestamp = formatTime defaultTimeLocale "%s%3q" now
        queryString = LBS.pack $ "recvWindow=10000"
                               <> "&timestamp=" <> timestamp
        signature = Text.pack $ show $ hmacSha256 (LBS.pack secret) queryString
        opts = defaults & header "X-MBX-APIKEY" .~ [BS.pack key]
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

account_ :: Wreq.Session -> ApiKey -> ApiSecret -> IO (BinanceHttpResponse (Response Account))
account_ session key secret = do
    now <- getCurrentTime

    let timestamp = formatTime defaultTimeLocale "%s%3q" now
        queryString =  "recvWindow=10000"
                               <> "&timestamp=" <> timestamp
        signature = Text.pack $ show $ hmacSha256 (LBS.pack secret) (LBS.pack queryString)
        opts = defaults & header "X-MBX-APIKEY" .~ [BS.pack key]
                        & param "signature" .~ [signature]

    httpGetWith session opts ("https://api.binance.com/api/v3/account" <> "?" <> queryString)
