module BookKeeper where

import BinanceSession
import Control.Concurrent (takeMVar)
import Control.Concurrent.MVar (putMVar)
import Binance
import Network.Wreq.Session ( Session )

-- runBookKeeper

recordBookerPrice :: BinanceSessionState
        -> IO (Either ([Book], WeightCount) FailureMessage)
recordBookerPrice bSess = do

    binanceRequest bSess booker
