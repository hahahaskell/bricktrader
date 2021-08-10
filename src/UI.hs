{-# LANGUAGE OverloadedStrings #-}

module UI (runTui, AppEvents) where

import Brick
import Brick.BChan (newBChan, writeBChan, BChan)
import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

import qualified Data.Vector as Vec
import qualified Data.Text.Internal as T
import qualified Data.Text as T

import Control.Monad (void, forever)
import Control.Concurrent ( threadDelay, forkIO, takeMVar )
import Control.Concurrent.MVar (putMVar)

import Text.Printf ( printf )
import Data.Text (Text, unpack, pack)
import Data.Time ( defaultTimeLocale, getZonedTime, formatTime )

import Binance
import BinanceSession
import Lib (BrickTraderConfig(BrickTraderConfig, symbols, apiKey, apiSecret))
import BookKeeper (recordBookerPrice)

data AppState = AppState
    { loggerContents :: [Text]
    , tickerContent :: String
    , binanceStatusContent :: String
    , weightCountContent :: String
    , binanceLoggerContents :: [Text]
    }
data AppName = Main

data AppEvents =
     LogEvent Text
     | BinanceLogEvent Text -- todo improve, too cheapp
     | StatusEvent SystemStatus
     | TickerEvent [Ticker]
     | WeightEvent WeightCount

initialState :: AppState
initialState =
    AppState { loggerContents = ["Welcome to BrickTrader. "]
             , tickerContent = ""
             , binanceStatusContent = "⏳"
             , weightCountContent = "0/1200"
             , binanceLoggerContents = []
             }

drawUI :: AppState -> [Widget ()]
drawUI s = [a]
    where
        a =
            hBox [str $ tickerContent s ]
            <=> B.hBorder
            <=> hBox [ hLimit 25 $ vLimit 5 $  withBorderStyle BS.unicodeRounded $ B.border $ C.center $ str "Position 1",
                    hLimit 25 $ vLimit 5 $  withBorderStyle BS.unicodeRounded $ B.border $ C.center $ str "Position 2",
                    hLimit 25 $ vLimit 5 $  withBorderStyle BS.unicodeRounded $ B.border $ C.center $ str "Position 3",
                    hLimit 25 $ vLimit 5 $  withBorderStyle BS.unicodeRounded $ B.border $ C.center $ str "Position 4",
                    hLimit 25 $ vLimit 5 $  withBorderStyle BS.unicodeRounded $ B.border $ C.center $ str "Position 5",
                    hLimit 25 $ vLimit 5 $  withBorderStyle BS.unicodeRounded $ B.border $ C.center $ str "Position 6",
                    hLimit 25 $ vLimit 5 $  withBorderStyle BS.unicodeRounded $ B.border $ C.center $ str "Position 7"
                    ]
            <=> hBox  [ withBorderStyle BS.unicode $ B.border (renderBottomUp (txtWrap <$> loggerContents s))
                      , hLimitPercent 35 (vBox [ withBorderStyle BS.unicodeRounded $ B.border ( C.center $ str "current assets"),
                                                 withBorderStyle BS.unicodeRounded $ B.border ( C.center $ str "pending buy orders"),
                                                 withBorderStyle BS.unicodeRounded
                                                  $ B.border $ vBox [ str ("Binance: " ++ binanceStatusContent s) <+> padLeft Max (str $ weightCountContent s)
                                                                    , B.hBorder
                                                                    , renderBottomUp (txtWrap <$> binanceLoggerContents s)
                                                                    ]
                                                ])
                    ]
            <=> withAttr "statusLine" (padRight Max $ str "[O] Order [Q] Quit [H] Help")

theMap :: AttrMap
theMap = attrMap V.defAttr
    [
        ("statusLine", V.white `on` V.blue)
    ]

handleEvent :: AppState -> T.BrickEvent () AppEvents -> T.EventM () (T.Next AppState)
handleEvent s (AppEvent (LogEvent l)) = continue $ s { loggerContents = l : loggerContents s }
handleEvent s (AppEvent (BinanceLogEvent l)) = continue $ s { binanceLoggerContents = l : binanceLoggerContents s }
handleEvent s (AppEvent (TickerEvent p)) = continue $ s { tickerContent = handleTickerEvent p }
handleEvent s (AppEvent (StatusEvent t)) = continue $ s { binanceStatusContent = handleStatusEvent t }
handleEvent s (AppEvent (WeightEvent w)) = continue $ s { weightCountContent = handleWeightEvent w }
handleEvent s (VtyEvent (V.EvKey (V.KChar 'Q') [])) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s _ = continue s

handleTickerEvent :: [Ticker] -> String
handleTickerEvent pr = "| " ++ foldMap format pr
    where
        format t = printf "%s $%.2f %c %.2f%% | " (symbol t) (lastPrice t) (arrow $ percent t) (percent t)
        symbol t = unpack $ tickerSymbol t
        percent t = read $ unpack (tickerPriceChangePercent t) :: Double
        lastPrice t =  read $ unpack (tickerLastPrice t) :: Double
        arrow p = if p < 0 then '⯆' else '⯅'

handleStatusEvent :: SystemStatus -> String
handleStatusEvent Online = "🟢"
handleStatusEvent (Offline _) = "🔴"
handleStatusEvent (Maintenance _) = "🚧"

handleWeightEvent :: WeightCount -> String
handleWeightEvent w = show w ++ "/1200"

theApp :: App AppState AppEvents ()
theApp = M.App { appDraw         = drawUI
               , appChooseCursor = neverShowCursor
               , appHandleEvent  = handleEvent
               , appAttrMap      = const theMap
               , appStartEvent   = return
               }

runTui :: BrickTraderConfig -> IO ()
runTui config = do
    cfg <- V.standardIOConfig
    vty <- V.mkVty cfg
    chan <- newBChan 10

    bSess@(BinanceSessionState m) <- newBinanceSession (apiSecret config) (apiKey config)
    _ <- healthCheck bSess -- connect

    -- setUncaughtExceptionHandler ""
    void $ forkIO $ forever $ healthCheckJob bSess chan (1 * 1000000)
    void $ forkIO $ forever $ tickerJob bSess chan (symbols config) (60 * 1000000)
    void $ forkIO $ forever $ bookKeeperJob bSess chan (symbols config) (5 * 1000000)

    void $ customMain vty (V.mkVty cfg) (Just chan) theApp initialState

bookKeeperJob :: BinanceSessionState  -> BChan AppEvents -> [Text] -> Int -> IO ()
bookKeeperJob bSess@(BinanceSessionState m) chan symbols delay = do
    r <- recordBookerPrice bSess symbols
    now <- getZonedTime

    case r of
      Left (book, weight) -> do
           writeBChan chan $ WeightEvent weight
           writeBChan chan $ BinanceLogEvent $ pack $ logDateTime now ++ "Got book keeper prices."
           pure ()
      Right s -> logger chan $  "Failed to get books: " ++ s

    threadDelay delay
    where
        logDateTime = formatTime defaultTimeLocale "%b %e %T > "

-- This job monitors the gateway endpoint for system outages, maintenance or high latency
healthCheckJob :: BinanceSessionState -> BChan AppEvents -> Int -> IO ()
healthCheckJob bSess@(BinanceSessionState m) chan delay = do
    state <- takeMVar m
    putMVar m state
    let prevStatus = status state

    (binanceStatus, latency) <- healthCheck bSess
    writeBChan chan $ StatusEvent binanceStatus

    case binanceStatus of
            Offline s -> logger chan "Binance is offline."
            Maintenance s -> logger chan $ "Binance is down for maintenance." ++ show s
            Online -> case prevStatus of
                Offline _ -> logger chan "Binance is online."
                Maintenance _ -> logger chan "Resumed after maintenance."
                Online -> pure ()

    threadDelay delay

tickerJob :: BinanceSessionState -> BChan AppEvents -> [Text] -> Int -> IO ()
tickerJob (BinanceSessionState m) chan symbols delay = do
    state <- takeMVar m
    putMVar m state

    if status state == Online then do
        prices <- ticker $ session state

        case prices of
            Success (p,w) -> do
                writeBChan chan $ TickerEvent $ filter match p
                writeBChan chan $ WeightEvent w
            Failure s  -> logger chan $ "Error fetching ticker prices: " ++ show s
    else
        pure ()

    threadDelay delay
    where
        match p = tickerSymbol p `elem` symbols

logger :: BChan AppEvents -> String -> IO ()
logger c m = do
        now <- getZonedTime
        writeBChan c $ LogEvent $ pack $ logDateTime now ++ m
        return ()
    where
        logDateTime = formatTime defaultTimeLocale "%b %e %T > "

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