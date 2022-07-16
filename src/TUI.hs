
module TUI where
-- module UI (runTui, AppEvents) where

-- import Brick
-- import Brick.BChan (newBChan, writeBChan, BChan)
-- import qualified Graphics.Vty as V
-- import qualified Brick.Main as M
-- import qualified Brick.Types as T
-- import qualified Brick.Widgets.List as L
-- import qualified Brick.Widgets.Border.Style as BS
-- import qualified Brick.Widgets.Border as B
-- import qualified Brick.Widgets.Center as C

-- import qualified Data.Vector as Vec
-- import qualified Data.Text.Internal as T
-- import qualified Data.Text as T

-- import Control.Monad (void, forever)
-- import Control.Concurrent ( threadDelay, forkIO, takeMVar )
-- import Control.Concurrent.MVar (putMVar)

-- import Text.Printf ( printf )
-- import Data.Text (Text, unpack, pack)
-- import Data.Time ( defaultTimeLocale, getZonedTime, formatTime )

-- import Binance
-- import Lib (BrickTraderConfig(BrickTraderConfig, symbols, apiKey, apiSecret), Millisecond)
-- -- import BookKeeper (recordBookerPrice)
-- import BookMaker

-- data AppState = AppState
--     { loggerContents :: [Text]
--     , tickerContent :: String
--     , systemStatusContent :: String
--     , latencyContent :: String
--     , orderWeightContent :: String
--     , weightCountContent :: String
--     , weightLimitContent :: String
--     , timeDeltaContent :: String
--     , symbolsCountContent :: String
--     , symbolsContent :: [String]
--     }
-- data AppName = Main

-- data AppEvents =
--      LogEvent Text
--      | StatusEvent SystemStatus
--      | TickerEvent [Ticker]
--      | WeightEvent WeightCount
--      | LatencyEvent Millisecond

-- initialState :: AppState
-- initialState =
--     AppState { loggerContents = ["You've hit the JackBot!"]
--              , tickerContent = ""
--              , systemStatusContent = ""
--              , latencyContent = "0ms"
--              , orderWeightContent = ""
--              , weightCountContent = ""
--              , weightLimitContent = ""
--              , timeDeltaContent = "+0.02s"
--              , symbolsCountContent = "0"
--              , symbolsContent = []
--              }

-- drawUI :: AppState -> [Widget ()]
-- drawUI s = [a]
--     where
--         a = withAttr "headerbarAttr" $
--             hBox [ str "BrickTrader"
--                  , padLeft Max $ hBox [ padLeftRight 1 (str "[") , str (latencyContent s) , padLeft (Pad 1) (str "]")
--                                       , padLeftRight 1 (str "[") , str (timeDeltaContent s) , padLeft (Pad 1) (str "]")
--                                       , padLeftRight 1 (str "[") , str (weightCountContent s) , padLeft (Pad 1) (str "]")
--                                       , padLeftRight 1 (str "[") , str (systemStatusContent s) , padLeft (Pad 1) (str "]")
--                                       ]
--                  ]
--             <=> withAttr "statusLine" (padRight Max $ str "[O] Order [Q] Quit [H] Help")

--             -- <+> padLeftRight 1 (str "[") , str (healthCheckContent s) , padLeft (Pad 1) (str "]")
--         --    <=> withAttr "statusLine" (padRight Max $ str "[O] Order [Q] Quit [H] Help")
--             -- B.border $
--             -- str "BrickTrader"
--             -- -- <+> padLeft Max $ hLimit 40
--             -- <+> hLimit 40 (str "this")
--             -- <+> (str (healthCheckContent s))
--             -- <+> C.hCenter (str "date")
--             -- <+> (str (weightCountContent s))
--             -- B.border $ hBox [ str (weightCountContent s), str (healthCheckContent s) ]

--             -- B.border $ padLeft Max $ str (healthCheckContent s)

--             --  str (tickerContent s)

--             -- hBox [str $ tickerContent s,
--                 --
--                 --  ]
--             -- <=> hBox  [ withBorderStyle BS.unicode $ B.border (renderBottomUp (txtWrap <$> loggerContents s))
--             --           , hLimitPercent 35 (vBox [ withBorderStyle BS.unicodeRounded $ B.border ( C.center $ str "current assets"),
--             --                                      withBorderStyle BS.unicodeRounded $ B.border ( C.center $ str "pending buy orders"),
--             --                                      withBorderStyle BS.unicodeRounded
--             --                                       $ B.border $ vBox [ str ("Binance: " ++ binanceStatusContent s) <+> padLeft Max (str $ weightCountContent s)
--             --                                                         , B.hBorder
--             --                                                         , renderBottomUp (txtWrap <$> binanceLoggerContents s)
--             --                                                         ]
--             --                                     ])
--             --         ]
--             -- <=> withAttr "statusLine" (padRight Max $ str "[O] Order [Q] Quit [H] Help")

-- theMap :: AttrMap
-- theMap = attrMap V.defAttr
--     [ ("headerbarAttr", V.brightBlack `on` V.brightWhite)
--     , ("statusbarAttr", V.white `on` V.blue)
--     ]

-- handleEvent :: AppState -> T.BrickEvent () AppEvents -> T.EventM () (T.Next AppState)
-- handleEvent s (AppEvent (LogEvent l)) = continue $ s { loggerContents = l : loggerContents s }
-- handleEvent s (AppEvent (TickerEvent p)) = continue $ s { tickerContent = handleTickerEvent p }
-- handleEvent s (AppEvent (StatusEvent t)) = continue $ s { systemStatusContent = handleStatusEvent t }
-- handleEvent s (AppEvent (WeightEvent w)) = continue $ s { weightCountContent = handleWeightEvent w }
-- handleEvent s (AppEvent (LatencyEvent m)) = continue $ s { latencyContent = handleLatencyEvent m }
-- handleEvent s (VtyEvent (V.EvKey (V.KChar 'Q') [])) = halt s
-- handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
-- handleEvent s _ = continue s

-- handleTickerEvent :: [Ticker] -> String
-- handleTickerEvent pr = "| " ++ foldMap format pr
--     where
--         format t = printf "%s $%.2f %c %.2f%% | " (symbol t) (lastPrice t) (arrow $ percent t) (percent t)
--         symbol t = unpack $ tickerSymbol t
--         percent t = read $ unpack (tickerPriceChangePercent t) :: Double
--         lastPrice t =  read $ unpack (tickerLastPrice t) :: Double
--         arrow p = if p < 0 then '⯆' else '⯅'

-- handleStatusEvent :: SystemStatus -> String
-- handleStatusEvent Online = "Online" -- "🟢"
-- handleStatusEvent (Offline _) =  "Offline" --"🔴"
-- handleStatusEvent (Maintenance _) = "Down" -- "🚧"

-- handleWeightEvent :: WeightCount -> String
-- handleWeightEvent w =  show w ++ "/1200"

-- handleLatencyEvent :: Millisecond -> String
-- handleLatencyEvent m = show m ++ "ms"


-- theApp :: App AppState AppEvents ()
-- theApp = M.App { appDraw         = drawUI
--                , appChooseCursor = neverShowCursor
--                , appHandleEvent  = handleEvent
--                , appAttrMap      = const theMap
--                , appStartEvent   = return
--                }

-- runTui :: BrickTraderConfig -> IO ()
-- runTui config = do
--     cfg <- V.standardIOConfig
--     vty <- V.mkVty cfg
--     chan <- newBChan 10

--     bSess@(BinanceSessionState m) <- newBinanceSession (Lib.apiSecret config) (Lib.apiKey config)
--     _ <- healthCheck bSess -- connect
--     oSess <- newBianceOrderState

--     -- setUncaughtExceptionHandler ""
--     void $ forkIO $ forever $ healthCheckJob bSess chan (1 * 1000000)
--     void $ forkIO $ forever $ tickerJob bSess chan (symbols config) (60 * 1000000)
--     -- void $ forkIO $ forever $ bookKeeperJob bSess oSess chan (60 * 1000000)
--     void $ forkIO $ forever $ bookMakerJob bSess chan (symbols config) (5 * 1000000)

--     void $ customMain vty (V.mkVty cfg) (Just chan) theApp initialState

-- bookKeeperJob :: BinanceSessionState  -> BinanceOrderState -> BChan AppEvents -> Int -> IO ()
-- bookKeeperJob bSess@(BinanceSessionState m) oSess@(BinanceOrderState mos) chan delay = do
--     r <- binanceExchangeInfo bSess oSess

--     -- case r of
--     --   Left (book, weight) -> do
--     --        writeBChan chan $ WeightEvent weight
--     --        writeBChan chan $ BinanceLogEvent $ pack $ logDateTime now ++ "Got book keeper prices."
--     --        pure ()
--     --   Right s -> logger chan $  "Failed to get books: " ++ s

--     threadDelay delay
--     where
--         -- logDateTime = formatTime defaultTimeLocale "%b %e %T > "

-- bookMakerJob :: BinanceSessionState  -> BChan AppEvents -> [Text] -> Int -> IO ()
-- bookMakerJob bSess@(BinanceSessionState m) chan symbols delay = do
--     r <- recordBookerPrice bSess symbols
--     now <- getZonedTime

--     case r of
--       Left (book, weight) -> do
--            writeBChan chan $ WeightEvent weight
--         --    writeBChan chan $ BinanceLogEvent $ pack $ logDateTime now ++ "Got book keeper prices."
--            pure ()
--       Right s -> pure ()

--     threadDelay delay
--     where
--         -- logDateTime = formatTime defaultTimeLocale "%b %e %T > "

-- -- Monitors the gateway endpoint for system outages, maintenance or high latency
-- healthCheckJob :: BinanceSessionState -> BChan AppEvents -> Int -> IO ()
-- healthCheckJob bSess@(BinanceSessionState m) chan delay = do
--     state <- takeMVar m
--     putMVar m state

--     let previousHealth = status state
--     (health, latency) <- healthCheck bSess
--     writeBChan chan $ StatusEvent health
--     writeBChan chan $ LatencyEvent latency

--     case (previousHealth, health) of
--         (Online, Offline _) -> logger chan "Binance is offline."
--         (Offline _, Online) -> logger chan "Binance is online."
--         (Maintenance _, Online) -> logger chan "Binance is online."
--         (_, Maintenance s) -> logger chan $ "Binance is down for maintenance." ++ show s
--         _ -> pure ()

--     threadDelay delay

-- tickerJob :: BinanceSessionState -> BChan AppEvents -> [Text] -> Int -> IO ()
-- tickerJob (BinanceSessionState m) chan symbols delay = do
--     state <- takeMVar m
--     putMVar m state

--     if status state == Online then do
--         prices <- ticker $ session state

--         case prices of
--             Success (p,w) -> do
--                 writeBChan chan $ TickerEvent $ filter match p
--                 writeBChan chan $ WeightEvent w
--             Failure s  -> logger chan $ "Error fetching ticker prices: " ++ show s
--     else
--         pure ()

--     threadDelay delay
--     where
--         match p = tickerSymbol p `elem` symbols

-- logger :: BChan AppEvents -> String -> IO ()
-- logger c m = do
--         now <- getZonedTime
--         writeBChan c $ LogEvent $ pack $ logDateTime now ++ m
--         return ()
--     where
--         logDateTime = formatTime defaultTimeLocale "%b %e %T > "

-- renderBottomUp :: [Widget n] -> Widget n
-- renderBottomUp ws =
--     Widget Greedy Greedy $ do
--         let go _ [] = return V.emptyImage
--             go remainingHeight (c:cs) = do
--                 cResult <- render c
--                 let img = image cResult
--                     newRemainingHeight = remainingHeight - V.imageHeight img
--                 if newRemainingHeight == 0
--                    then return img
--                    else if newRemainingHeight < 0
--                         then return $ V.cropTop remainingHeight img
--                         else do
--                             rest <- go newRemainingHeight cs
--                             return $ V.vertCat [rest, img]

--         ctx <- getContext
--         img <- go (availHeight ctx) ws
--         render $ fill ' ' <=> raw img