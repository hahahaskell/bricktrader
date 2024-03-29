module Service.ClientManager
  ( createService
  , Options (..)
  , Hooks (..)
  , Service (..)
  )
 where

import Types
import Client.Binance
import Data.Text (Text)
import Control.Lens ((^.))
import Control.Concurrent.MVar (MVar, takeMVar, putMVar, newMVar)
import Util ( getWallTime, getHeader )
import System.Clock (Clock(Monotonic), getTime)
import Network.HTTP.Client (Response)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Network.Wreq.Lens (responseBody)

data Options = Options
    { sleep :: Int
    }

data Service = Service
    { connect :: IO SystemStatus
    , disconnect :: IO SystemStatus
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

data Hooks = Hooks
  { connected :: IO ()
  , disconnected :: Text -> IO ()
  -- , latency :: Latency -> IO
  }

createService :: Options -> Client -> Hooks -> IO Service
createService options binance hooks = do

  m <- newMVar BinanceHealth
        { latency = -1
        , system = Offline "Inactive"
        , time = -1
        , weight = -1
        }
  let health = BinanceHealthState m

  return Service
      { connect = healthCheck_ health hooks binance
      , disconnect = disconnect_ health hooks
      , systemTime = systemTime_ health binance
      , price = binanceRequest health . binance.price
      , prices = binanceRequest health binance.prices
      , exchangeInfo = binanceRequest health binance.exchangeInfo
      , ticker = binanceRequest health binance.ticker
      , bookTicker = binanceRequest health binance.bookTicker
      , order = binanceRequest health . binance.order
      , openOrders = binanceRequest health . binance.openOrders
      , orderCount = binanceRequest health binance.orderCount
      , account = binanceRequest health binance.account
      }

newtype BinanceHealthState = BinanceHealthState (MVar BinanceHealth)
data BinanceHealth = BinanceHealth { latency :: Latency
                                   , system :: SystemStatus
                                   , time :: EpochTime
                                   , weight :: WeightCount
                                   } deriving (Show)

getWeightCountHeader :: Response body -> WeightCount
getWeightCountHeader r = fromMaybe defaultWeightCount $ readMaybe $ getHeader r "x-mbx-used-weight-1m"
        where defaultWeightCount = -1

binanceRequest ::
 BinanceHealthState ->
 IO (BinanceHttpResponse (Response a)) ->
 IO (BinanceResult a BinanceFailureResult)
binanceRequest (BinanceHealthState m) request = do
    health <- takeMVar m
    putMVar m health

    if health.system == Online
    then do
        start <- getTime Monotonic
        response <- request
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

mkFailure :: BinanceHttpResponse a -> BinanceFailureResult
mkFailure SecurityViolation = Halt "Binance's WAF has rejected the request."
mkFailure (IpBan s) = Halt $ "IP banned for " <> show s
mkFailure (Fault s) = Halt s
mkFailure (BackOff s) = Sleep s
mkFailure (ConnectionFailed s) = Retry s
mkFailure _ = Halt "Unhandled failure"

healthCheck_ :: BinanceHealthState -> Hooks -> Client -> IO SystemStatus
healthCheck_ (BinanceHealthState m) hooks client = do
    status <- client.healthCheck

    health <- takeMVar m
    putMVar m health { system = status }
    sendHook (health.system, status)

    return status
    where
      sendHook (Online, Online) = pure()
      sendHook (_, Online) = hooks.connected
      sendHook (Online, Maintenance txt) = hooks.disconnected $ "Binance is down for maintance: " <> txt
      sendHook _ = hooks.disconnected "Binance is unreachable."

disconnect_ :: BinanceHealthState -> Hooks -> IO SystemStatus
disconnect_ (BinanceHealthState m) hooks = do
  let status = Offline "User action"
  health <- takeMVar m
  putMVar m health { system = status }
  sendHook (health.system, status)

  return status

  where
    sendHook (Online, Offline txt) = hooks.disconnected txt
    sendHook _ = pure ()

systemTime_ :: BinanceHealthState -> Client -> IO (BinanceResult SystemTime BinanceFailureResult)
systemTime_ (BinanceHealthState m) client  = do
    response <- client.systemTime

    case response of
      Ok r -> do
            health <- takeMVar m
            putMVar m health { time = (r ^. responseBody).serverTime }
            return $ Success (r ^. responseBody)
      _ -> return . Failure $ Retry "" --TODO make failure?
