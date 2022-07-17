module Run where

import Import
import qualified Client.Binance as Binance
import qualified Worker.HealthMonitoring as HealthMonitoring
import Worker.HealthMonitoring
import Control.Concurrent (threadDelay, forkIO)
import qualified Service.BinanceClientManager as ClientManager
import Control.Monad (forever)
import TUI (runTui)

data AppState = AppState
  { appClient  :: ClientManager.Service
  }

main :: IO ()
main = do
  let binanceOptions = BinanceOptions
        { apiKey = ""
        , apiSecret = ""
        }

  let clientManagerOptions = ClientManager.BinanceClientManagerOptions
        {
          sleep = 1000
        }

--   let healthOptions = HealthMonitoringOptions
--         { sleep = 10000
--         }
  let hooks = ClientManager.Hooks
        { disconnected = \_ -> print "d/c"
        , connected = \() -> print "pong"
        }

  binance <- Binance.createClient binanceOptions
  clientManager <- ClientManager.createService clientManagerOptions binance hooks
--   health <- HealthMonitoring.createWorker healthOptions binance healthHooks

  void <- forkIO . forever $ clientManager.healthCheck

--   health.run

  let appState = AppState
        { appClient = clientManager
        }

  runTui appState
  -- threadDelay maxBound
  return ()

