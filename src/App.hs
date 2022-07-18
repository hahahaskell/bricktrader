module App where

import Types
import TUI
import Brick
import Brick.BChan (newBChan, writeBChan, BChan)
import qualified Graphics.Vty as V
import qualified Client.Binance as Binance
import qualified Worker.HealthMonitoring as HealthMonitoring
import Worker.HealthMonitoring
import Control.Concurrent (threadDelay, forkIO)
import qualified Service.BinanceClientManager as ClientManager
import Control.Monad (forever, void)
import Data.Text (Text)
import Client.Binance (SystemStatus, Ticker)

data AppState = AppState
  { appClient  :: ClientManager.Service
  }

appName :: String
appName = "BrickTrader"

appMain :: IO ()
appMain = do
  let binanceOptions = BinanceOptions
        { apiKey = ""
        , apiSecret = ""
        }

  let clientManagerOptions = ClientManager.BinanceClientManagerOptions
        {
          sleep = 1000
        }

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

runTui :: AppState -> IO ()
runTui state = do
    cfg <- V.standardIOConfig
    vty <- V.mkVty cfg
    chan <- newBChan 10

    let tuiApp  = App
          { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appAttrMap      = const theMap
          , appStartEvent   = return
          }

    let appContent = TUI.AppContent
          { loggerContents = ["Brick Trader"]
          , tickerContent = ""
          , systemStatusContent = ""
          , latencyContent = "0ms"
          , orderWeightContent = ""
          , weightCountContent = ""
          , weightLimitContent = ""
          , timeDeltaContent = "+0.02s"
          , symbolsCountContent = "0"
          , symbolsContent = []
          }

    -- bSess@(BinanceSessionState m) <- newBinanceSession (Lib.apiSecret config) (Lib.apiKey config)
    -- _ <- healthCheck bSess -- connect
    -- oSess <- newBianceOrderState

    -- setUncaughtExceptionHandler ""
    -- void $ forkIO $ forever $ healthCheckJob bSess chan (1 * 1000000)
    -- void $ forkIO $ forever $ tickerJob bSess chan (symbols config) (60 * 1000000)
    -- void $ forkIO $ forever $ bookKeeperJob bSess oSess chan (60 * 1000000)
    -- void $ forkIO $ forever $ bookMakerJob bSess chan (symbols config) (5 * 1000000)

    void $ customMain vty (V.mkVty cfg) (Just chan) tuiApp appContent