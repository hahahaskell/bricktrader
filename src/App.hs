module{-# LANGUAGE DuplicateRecordFields, DataKinds, FlexibleInstances, TypeApplications, FlexibleContexts, MultiParamTypeClasses, TypeFamilies, TypeOperators, GADTs, UndecidableInstances #-}App where
{- HLINT ignore "Redundant bracket" -}

import TUI
import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Graphics.Vty as V
import qualified Client.Binance as Binance
import Control.Concurrent (forkIO)
import qualified Service.ClientManager as ClientManager
import Control.Monad (void)
import qualified Service.ExchangeInfo as ExchangeInfo
import qualified Service.Logger as Logger
import Service.Logger (Verbosity(..))
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import TUI (AppContent(contentLogger, contentSymbolsList))

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

-- TODO appContent should be included in App?
  let appContent = TUI.AppContent
        { contentLogger = ["Brick Trader"]
        , contentTicker = ""
        , contentStatus = ""
        , contentLatency = "0ms"
        , contentWeightCount = ""
        , contentWeightLimit = ""
        , contentTimeDelta = "+0.02s"
        , contentSymbols = []
        , contentSymbolsList = L.list () (Vec.fromList []) 1
        , contentSelectedSymbol = ""
        }

  let clientManagerHooks = ClientManager.Hooks
        { disconnected = \_ -> writeBChan chan $ LogEvent "d/c"
        , connected = writeBChan chan $ LogEvent "online"
        }

  let binanceOptions = Binance.BinanceOptions
        { apiKey = ""
        , apiSecret = ""
        }
  let clientManagerOptions = ClientManager.Options
        { sleep = 1000 }
  let exchangeInfoOptions = ExchangeInfo.Options { }
  let exchangeInfoHooks = ExchangeInfo.Hooks { }
  let loggerOptions = Logger.Options
            { verbosity = Information
            }
  let loggerHooks = Logger.Hooks
            { logger = (writeBChan chan . LogEvent)
            }

  logger <- Logger.createService loggerOptions loggerHooks
  binance <- Binance.createClient binanceOptions
  clientManager <- ClientManager.createService clientManagerOptions binance clientManagerHooks
  exchangeInfo <- ExchangeInfo.createService exchangeInfoOptions exchangeInfoHooks clientManager

  void $ forkIO $ do
    _ <- clientManager.connect

    -- init
    symbols <- exchangeInfo.getSymbols
    writeBChan chan $ SymbolListInitEvent symbols

    -- run jobs

--     let appContent = appContent { contentSymbolsList = L.list () (Vec.fromList []) 1 }

--     threadDelay $ 5 * oneSecond
--     _ <- clientManager.disconnect
--     logger.logInfo $ Text.concat symbols
    return ()

  void $ customMain vty (V.mkVty cfg) (Just chan) app appContent
  -- return()

