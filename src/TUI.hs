module TUI
  ( AppContent (..),
    AppEvents (..),
    drawUI,
    handleEvent,
    theMap
  )
where

import Types
import Brick
import Brick.BChan (writeBChan, BChan)
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import Data.Text (Text, pack)
import Data.Time ( defaultTimeLocale, getZonedTime, formatTime )
import Client.Binance (SystemStatus (..), Ticker)
import qualified Brick.Widgets.Center as C
import qualified Data.Vector as Vec
import Graphics.Vty (withForeColor)
import qualified Data.Char as Char
import Control.Lens ((^.))

data AppContent = AppContent
    { contentLogger :: [Text]
    , contentTicker :: String
    , contentStatus :: String
    , contentLatency :: String
    , contentWeightCount :: String
    , contentWeightLimit :: String
    , contentTimeDelta :: String
    , contentSymbols :: [String]
    , contentSymbolsList :: L.List () Text
    }
$(suffixLenses ''AppContent)

data AppEvents =
     LogEvent Text
     | StatusEvent SystemStatus
     | TickerEvent [Ticker]
     | WeightEvent WeightCount
     | LatencyEvent Millisecond
     | SymbolListInitEvent [Text]
     | SymbolListAddEvent Text
     | SymbolListRemoveEvent Text

drawUI :: AppContent -> [Widget ()]
drawUI s = [ui]
    where
        ui =
            hBox [ withAttr "headerbarAttr" $ str "BrickTrader"
                 , withAttr "headerbarAttr" $ padLeft Max $ hBox [
                                      padLeftRight 1 (str "[") , str (contentLatency s) , padLeft (Pad 1) (str "]")
                                     , padLeftRight 1 (str "[") , str (contentTimeDelta s) , padLeft (Pad 1) (str "]")
                                     , padLeftRight 1 (str "[") , str (contentWeightLimit s) , padLeft (Pad 1) (str "]")
                                     , padLeftRight 1 (str "[") , str (contentStatus s) , padLeft (Pad 1) (str "]")
                                     ]
                ]
            <=> hBox [ str $ contentTicker s ]
            <=> hBox [  vBox [ withBorderStyle BS.unicode $ B.border (renderGraph (txtWrap <$> ["blahhh"]))
                             , vLimitPercent 20 $ withBorderStyle BS.unicode $ B.border (renderBottomUp (txtWrap <$> contentLogger s))
                             ]
                     , hLimitPercent 20 (vBox [
                        --  withBorderStyle BS.unicode $ B.border $ vBox [ str ("Binance: " ++ contentStatus s) <+> padLeft Max (str $ contentWeightCount s) , B.hBorder ],
                         withBorderStyle BS.unicode $ B.border $ L.renderList drawListElement True s.contentSymbolsList
                                              ]
                                        )
                     ]
            <=> withAttr "statusbarAttr" (padRight Max $ str "[O] Order [Q] Quit [?] Help")

        drawListElement :: (Show a) => Bool -> a -> Widget ()
        drawListElement sel a = str $ filter Char.isLetter (show a)
                -- selected s = if sel then withAttr

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ ("headerbarAttr", V.brightBlack `on` V.brightWhite)
    , ("statusbarAttr", V.white `on` V.blue)
    , ("loggerAttr", V.white `on` V.black)
    , (L.listAttr, V.defAttr `withForeColor` V.white)
    , (L.listSelectedAttr, V.blue `on` V.white)
    ]

handleEvent :: AppContent -> T.BrickEvent () AppEvents -> T.EventM () (T.Next AppContent)
handleEvent s (AppEvent (LogEvent l)) = continue $ s { contentLogger = l : contentLogger s }
-- handleEvent s (AppEvent (TickerEvent p)) = continue $ s { tickerContent = handleTickerEvent p }
handleEvent s (AppEvent (StatusEvent t)) = continue $ s { contentStatus = handleStatusEvent t }
handleEvent s (AppEvent (SymbolListInitEvent t)) = continue $ s { contentSymbolsList = L.list () (Vec.fromList t) 1}
handleEvent s (AppEvent (WeightEvent w)) = continue $ s { contentWeightCount = handleWeightEvent w }
handleEvent s (AppEvent (LatencyEvent m)) = continue $ s { contentLatency = handleLatencyEvent m }
handleEvent s (VtyEvent (V.EvKey (V.KChar 'Q') [])) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s (VtyEvent e) = continue =<< handleEventLensed s contentSymbolsListL (L.handleListEvent) e
handleEvent s _ = continue s

-- handleTickerEvent :: [Ticker] -> String
-- handleTickerEvent pr = "| " ++ foldMap format pr
--     where
--         format t = printf "%s $%.2f %c %.2f%% | " (symbol t) (lastPrice t) (arrow $ percent t) (percent t)
--         symbol t = unpack $ tickerSymbol t
--         percent t = read $ unpack (tickerPriceChangePercent t) :: Double
--         lastPrice t =  read $ unpack (tickerLastPrice t) :: Double
--         arrow p = if p < 0 then '⯆' else '⯅'

handleStatusEvent :: SystemStatus -> String
handleStatusEvent Online = "Online" -- "🟢"
handleStatusEvent (Offline _) =  "Offline" --"🔴"
handleStatusEvent (Maintenance _) = "Down" -- "🚧"

handleWeightEvent :: WeightCount -> String
handleWeightEvent w =  show w ++ "/1200"

handleLatencyEvent :: Millisecond -> String
handleLatencyEvent m = show m ++ "ms"

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

logger :: BChan AppEvents -> String -> IO ()
logger c m = do
        now <- getZonedTime
        writeBChan c $ LogEvent $ pack $ logDateTime now ++ m
        return ()
    where
        logDateTime = formatTime defaultTimeLocale "%b %e %T > "

renderGraph :: [Widget n] -> Widget n
renderGraph ws =
    Widget Greedy Greedy $ do
        ctx <- getContext
        let a = ctx^.attrL
        render $ fill ' '

renderBottomUp :: [Widget n] -> Widget n
renderBottomUp ws =
    Widget Greedy Greedy $ do
        let go _ [] = return V.emptyImage
            go remainingHeight (c:cs) = do
                cResult <- render c
                let img = image cResult
                    newRemainingHeight = remainingHeight - V.imageHeight img
                if newRemainingHeight == 0
                   then return img
                   else if newRemainingHeight < 0
                        then return $ V.cropTop remainingHeight img
                        else do
                            rest <- go newRemainingHeight cs
                            return $ V.vertCat [rest, img]

        ctx <- getContext
        img <- go (availHeight ctx) ws
        render $ fill ' ' <=> raw img