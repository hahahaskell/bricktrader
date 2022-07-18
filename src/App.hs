module App where

import Types
import TUI
import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Graphics.Vty as V
import qualified Client.Binance as Binance
import Control.Concurrent (forkIO)
import qualified Service.ClientManager as ClientManager
import Control.Monad (void)
import qualified Service.ExchangeInfo as ExchangeInfo
import qualified Data.Text as Text

data AppState = AppState
  { appClient  :: ClientManager.Service
  }

appName :: String
appName = "BrickTrader"

appMain :: IO ()
appMain = do
  cfg <- V.standardIOConfig
  vty <- V.mkVty cfg
  chan <- newBChan 5 -- will block when exceeded

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

  let clientManagerHooks = ClientManager.Hooks
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
  let exchangeInfoOptions = ExchangeInfo.ExchangeInfoOptions
        {
        }
  let exchangeInfoHooks = ExchangeInfo.Hooks {}

  binance <- Binance.createClient binanceOptions
  clientManager <- ClientManager.createService clientManagerOptions binance clientManagerHooks
  exchangeInfo <- ExchangeInfo.createService exchangeInfoOptions exchangeInfoHooks clientManager

  void $ forkIO $ do
    _ <- clientManager.connect
--     threadDelay $ 5 * oneSecond
--     _ <- clientManager.disconnect
    symbols <- exchangeInfo.getSymbols
    writeBChan chan $ LogEvent $ Text.concat symbols
    return ()

  void $ customMain vty (V.mkVty cfg) (Just chan) app appContent
  -- return()

