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
import qualified Service.ClientManager as ClientManager
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
  cfg <- V.standardIOConfig
  vty <- V.mkVty cfg
  chan <- newBChan 5 -- chosen by dice throw haha, will block when exceeded

  let app = App
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

  let hooks = ClientManager.Hooks
        { disconnected = \_ -> writeBChan chan $ LogEvent "d/c"
        , connected = writeBChan chan $ LogEvent "online"
        }

  let binanceOptions = BinanceOptions
        { apiKey = ""
        , apiSecret = ""
        }
  let clientManagerOptions = ClientManager.ClientManagerOptions
        { sleep = 1000
        }

  binance <- Binance.createClient binanceOptions
  clientManager <- ClientManager.createService clientManagerOptions binance hooks

  void $ forkIO $ forever $ do
    _ <- clientManager.connect
    threadDelay $ 5 * oneSecond
    _ <- clientManager.disconnect
    return ()

  -- void . forkIO . forever $ do
  --    writeBChan chan $ LogEvent "test"
  --    threadDelay $ 5 * oneSecond

  void $ customMain vty (V.mkVty cfg) (Just chan) app appContent

    -- setUncaughtExceptionHandler ""
    -- void $ forkIO $ forever $ healthCheckJob bSess chan (1 * 1000000)
    -- void $ forkIO $ forever $ tickerJob bSess chan (symbols config) (60 * 1000000)
    -- void $ forkIO $ forever $ bookKeeperJob bSess oSess chan (60 * 1000000)
    -- void $ forkIO $ forever $ bookMakerJob bSess chan (symbols config) (5 * 1000000)
