module Run where

import Import
import qualified Client.Binance as Binance
import qualified Worker.HealthMonitoring as HealthMonitoring
import Worker.HealthMonitoring
import Control.Concurrent (threadDelay)

data App = App
  { appBinance :: Binance.Client -- try not to require this
  }

main :: IO ()
main = do
  let binanceOptions = BinanceOptions
        { apiKey = ""
        , apiSecret = ""
        }

  let healthOptions = HealthMonitoringOptions
        { sleep = 10000
        }
  let healthHooks = HealthMonitoring.Hooks
        { disconnected = \_ -> print "d/c"
        , connected = \() -> print "pong"
        }

  binance <- Binance.createClient binanceOptions
  health <- HealthMonitoring.createWorker healthOptions binance healthHooks

  health.run

  let app = App
        { appBinance = binance
        }

  threadDelay maxBound
  return ()

