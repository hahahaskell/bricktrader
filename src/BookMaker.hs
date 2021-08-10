{-# LANGUAGE OverloadedStrings #-}
module BookMaker where

import Lib
import Binance
import Control.Concurrent (MVar, newMVar, takeMVar, putMVar)
import BinanceSession
import Data.Time (formatTime, getCurrentTime, secondsToDiffTime, diffUTCTime)
import Data.Time.Format (defaultTimeLocale)
import Data.Foldable (find)

data OrderState = OrderState { orderCount :: OrderCount
                             , orderCountLimit :: OrderCountLimit
                             , weightCount :: WeightCount
                             , weightCountLimit :: WeigthCountLimit
                             , delta :: Millisecond
                             }

newtype BinanceOrderState = BinanceOrderState (MVar OrderState)

newBianceOrderState :: IO BinanceOrderState
newBianceOrderState = do
    m <- newMVar OrderState { orderCount = 0
                            , orderCountLimit = 0
                            , weightCount = 0
                            , weightCountLimit = 0
                            , delta = 0
                            }
    return $ BinanceOrderState m

binanceExchangeInfo :: BinanceSessionState -> BinanceOrderState -> IO ()
binanceExchangeInfo bSess (BinanceOrderState m) = do
    now <- getCurrentTime
    result <- binanceRequest bSess exchangeInfo

    state <- takeMVar m
    case result of
        Left (Exchangeinfo _ t l s) -> do
             let timestamp = read $ formatTime defaultTimeLocale "%s" now :: Int
             let serverTimestamp = truncate $ fromIntegral t / 1000
             let delta = timestamp - serverTimestamp

             let orderWeight = find (\r -> ratelimitsRateLimitType r == "ORDERS" && ratelimitsInterval r == "MINUTE") l

        --      putMVar m state { }

             return ()
        Right e ->  return ()

    return ()
    where
        calcDelta = 0

-- s =  truncate $ millisecepoch / 1000
-- a = secondsToDiffTime s
-- formatTime defaultTimeLocale "%y%B%D %H:%M:%S - %s" a