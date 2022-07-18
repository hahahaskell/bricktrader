module Worker.HealthMonitoring where

-- import Import
-- import qualified Client.Binance as Binance
-- import Control.Monad (void, forever)
-- import Control.Concurrent (forkIO, threadDelay)
-- import Data.Text (Text)

-- data HealthMonitoringOptions = HealthMonitoringOptions
--     { sleep :: Int
--     }

-- data Service = Service
--     {
--       run :: IO ()
--     }

-- data Hooks = Hooks
--   { disconnected :: Text -> IO ()
--   , connected :: () -> IO ()
--   , error :: Text -> IO ()
--   }

-- createWorker :: HealthMonitoringOptions -> Binance.Client -> Hooks -> IO Service
-- createWorker options binance hooks = do




--   return Service
--       {
--         run = run_ options binance hooks
--       }

-- run_ :: HealthMonitoringOptions -> Binance.Client -> Hooks -> IO ()
-- run_ options binance hooks = do

--  -- could run this and the timer, should work seemslessly
--   void . forkIO . forever $ checkGatewayHealth binance hooks >> threadDelay 500
--   -- void . forkIO . forever $

-- -- The Binance module maintans a internal health monitoring from the result of this call
-- checkGatewayHealth :: Binance.Client -> Hooks -> IO ()
-- checkGatewayHealth binance hooks = do
--   health <- binance.healthCheck

--   case health of
--     Binance.Online -> hooks.connected ()
--     Binance.Maintenance txt -> undefined
--     Binance.Offline txt -> hooks.disconnected txt
--   return ()

-- checkSystemStatus :: Binance.Client -> Hooks -> IO ()
-- checkSystemStatus binance hooks = do
--   status <- binance.systemTime

--   case status of
--     Binance.Success st -> undefined
--     Binance.Failure e -> case e of
--       Binance.Halt s -> undefined
--       Binance.Sleep n -> undefined
--       Binance.Retry s -> undefined
--   return ()
