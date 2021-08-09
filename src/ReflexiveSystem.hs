{-# LANGUAGE OverloadedStrings #-}
module ReflexiveSystem where

import Binance
import Data.Text (Text)
import qualified Network.Wreq.Session as S

-- build reflexive system

exchangeJob = undefined


-- mkSession :: IO Session
-- mkSession = do
--     apiSess <- mkApiSession

--     m <- newEmptyMVar

--     return Session { api = apiSess }

-- get system status instead
-- getSystemStatus :: Session -> IO Session
-- getSystemStatus sess = do
--     let api = apiSession sess

--     status <- systemStatus api

--     return sess {  }


 -- job
-- getExchangeInfo
--     binanceStatus <- systemStatus sess
--     writeBChan chan $ StatusEvent binanceStatus
--     case binanceStatus of
--         Offline s -> logger chan "Binance is offline."
--         Maintenance s -> logger chan $ "Binance is down for maintenance." ++ show s
--         Online -> do
--                     logger chan "Binance is online."
--                     binanceExchangeInfo <- exchangeInfo sess

--                     case binanceExchangeInfo of
--                         Success (Exchangeinfo a b c s) -> logger chan $ "Found " ++ show (length s) ++ " symbols"
--                         Failure b -> logger chan "Unable to get binance exchange info"

--                     return ()

-- read timestamp
-- s =  truncate $ millisecepoch / 1000
-- a = secondsToDiffTime s
-- formatTime defaultTimeLocale "%y%B%D %H:%M:%S - %s" a

-- timer for system status report ms - improve jsno

--  bring back system time and calculate diff on current system



-- mk url
