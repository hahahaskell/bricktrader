module BookKeeper where

import BinanceSession
import Control.Concurrent (takeMVar)
import Control.Concurrent.MVar (putMVar)
import Binance
import Network.Wreq.Session ( Session )
import Data.Text (Text)

-- runBookKeeper


recordBookerPrice :: BinanceSessionState -> [Text]
        -> IO (Either ([Book], WeightCount) FailureMessage)
recordBookerPrice bSess symbols = do

    -- maybe not worth the retry logic as it's time sensitive
    result <- binanceRequest bSess booker

    case result of
      Left (b, w) -> appendFile path $ show (filter match b) ++ "\n"
      Right s -> pure ()

    return result
    where
        match p = bookSymbol p `elem` symbols
        path = "pricebook.json"
