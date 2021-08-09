module BinanceSession where

import Lib
import Binance
import Control.Concurrent.MVar (takeMVar, MVar, newMVar)
import Control.Concurrent (putMVar, threadDelay)
import System.Clock (getTime, Clock (Monotonic), diffTimeSpec, TimeSpec (nsec))
import qualified Network.Wreq.Session as S
import Brick.BChan
import Network.Wreq.Session (Session)

type RetryAttempt = Int

data SessionState = SessionState { session  :: S.Session
                                 , latency :: Latency
                                 , status :: SystemStatus
                                 , secret :: String
                                 , key :: String
                                 }
                   deriving (Show)
newtype BinanceSessionState = BinanceSessionState (MVar SessionState)

newBinanceSession :: String -> String -> IO BinanceSessionState
newBinanceSession secret key = do
    bSession <- mkSession
    s <- newMVar SessionState { session = bSession
                              , latency = -1
                              , status = Offline "Inactive"
                              , secret = secret
                              , key = key
                              }
    return $ BinanceSessionState s

healthCheck :: BinanceSessionState -> IO (SystemStatus, Millisecond)
healthCheck (BinanceSessionState m) = do
    state <- takeMVar m
    putMVar m state

    start <- getTime Monotonic
    status <- systemStatus $ session state
    end <- getTime Monotonic

    let diffNsec = nsec $ end `diffTimeSpec` start
        diffMsec = fromIntegral $ diffNsec `div` 1000000

    state <- takeMVar m
    putMVar m state { status = status, latency = diffMsec }

    return (status, diffMsec)

defaultRetryAttempts :: Int
defaultRetryAttempts = 3

defaultRetryDelay :: Second
defaultRetryDelay = 1

binanceRequest
    :: BinanceSessionState
    -> (Session -> IO (BinanceResult a BinanceFailureResult))
    -> IO (Either a FailureMessage)
binanceRequest (BinanceSessionState m) request = do
    state <- takeMVar m
    putMVar m state

    if status state == Online
        then do
            failSafeRequest (session state) request defaultRetryAttempts []
        else return $ Right "Binance offline."

failSafeRequest
    :: Session
    -> (Session -> IO (BinanceResult a BinanceFailureResult))
    -> RetryAttempt
    -> FailureMessage
    -> IO (Either a FailureMessage)
failSafeRequest session request attempts message = if attempts > 0
    then try
    else return $ Right $ message ++ "Retry attempts exceeded."
  where
    try = do
        result <- request session

        case result of
            Success a              -> return $ Left a
            Failure (Halt failure) -> return $ Right failure
            Failure (Sleep seconds) ->
                failSafeRequest session request (attempts - 1)
                    $  "Sleept for "
                    ++ show seconds
                    ++ ".\n"
                    ++ message
            Failure (Retry reason) ->
                failSafeRequest session request (attempts - 1)
                    $  reason
                    ++ message

-- fix me
-- Sleep seconds -> threadDelay defaultRetryDelay * 1000000 >> failSafeRequest session (attempts -1) $ "Sleept for " ++ show seconds