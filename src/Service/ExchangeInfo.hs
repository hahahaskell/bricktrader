module Service.ExchangeInfo where

import Types
import Control.Concurrent (newMVar, MVar, takeMVar)
import qualified Service.ClientManager as ClientManager
import Client.Binance
import Data.Time (getCurrentTime)
import Util (utcTimeToEpochTime)
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as Text

data Options = Options
    {
    }

data Service = Service
    {
      getSymbols :: IO [Text]
    }

data Hooks = Hooks
    {
    }

newtype CacheState = CacheState (MVar Cache)
data Cache = Cache
    { cachedAt :: EpochTime
    , exchangeInfo :: Exchangeinfo
    }

initExchangeInfo :: Exchangeinfo
initExchangeInfo = Exchangeinfo
          { timezone = ""
          , serverTime = 0
          , rateLimits = []
          , symbols = []
          }

createService :: Options -> Hooks -> ClientManager.Service -> IO Service
createService options hooks client = do

  m <- newMVar Cache
      { cachedAt = 0
      , exchangeInfo = initExchangeInfo
      }

  let cache = CacheState m

  return Service
      {
        getSymbols = getSymbols_ cache hooks client
      }

getExchangeInfo :: CacheState -> Hooks -> ClientManager.Service -> IO Exchangeinfo
getExchangeInfo (CacheState m) hooks client = do

  utc <- getCurrentTime
  let time = utcTimeToEpochTime utc

  cache <- takeMVar m

  if cacheExpired time cache
  then fetch
  else pure cache.exchangeInfo

  where
    cacheExpired t c = t > (c.cachedAt + 60 * 10000)

    fetch :: IO Exchangeinfo
    fetch = do
      result <- client.exchangeInfo
      return $ case result of
        Success ex -> ex
        Failure bfr -> error $ "Failed to fetch exchange info from Binance." <> show bfr

refresh_ :: CacheState -> Hooks -> ClientManager.Service -> IO ()
refresh_ cache hooks client = do
  info <- getExchangeInfo cache hooks client

  -- get the exchange info then event the changes
  -- to be manually run or on a schedule

  return ()


getSymbols_ :: CacheState -> Hooks -> ClientManager.Service -> IO [Text]
getSymbols_ cache hooks client = do
  info <- getExchangeInfo cache hooks client
  return $ map (\s -> s.symbol) info.symbols
