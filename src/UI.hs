{-# LANGUAGE OverloadedStrings #-}

module UI (runTui) where

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
import Control.Concurrent (threadDelay, forkIO)

import Text.Printf
import Data.Text (Text, unpack, pack)
import Data.Time ( defaultTimeLocale, getZonedTime, formatTime )

import Binance
import Network.Wreq.Session (Session)
import Lib (BrickTraderConfig(BrickTraderConfig))

data AppState = AppState
    { loggerContents :: [Text]
    , tickerContent :: String
    , binanceStatusContent :: String
    , weightCountContent :: String
    }
data AppName = Main

data AppEvents =
     LogEvent Text
     | StatusEvent SystemStatusResponse
     | TickerEvent [Ticker]
     | WeightEvent WeightCount

initialState :: AppState
initialState =
    AppState { loggerContents = ["Welcome to BrickTrader. "]
             , tickerContent = ""
             , binanceStatusContent = "⏳"
             , weightCountContent = "0/1200"
             }

drawUI :: AppState -> [Widget ()]
drawUI s = [a]
    where
        a =
            hBox [str $ tickerContent s, padLeft Max $ str $ weightCountContent s ++ " " ++ binanceStatusContent s ++ "  " ]
            <=> B.hBorder
            <=> hBox [ hLimit 25 $ vLimit 5 $  withBorderStyle BS.unicodeRounded $ B.border $ C.center $ str "Investment 1",
                    hLimit 25 $ vLimit 5 $  withBorderStyle BS.unicodeRounded $ B.border $ C.center $ str "Investment 2",
                    hLimit 25 $ vLimit 5 $  withBorderStyle BS.unicodeRounded $ B.border $ C.center $ str "Investment 3",
                    hLimit 25 $ vLimit 5 $  withBorderStyle BS.unicodeRounded $ B.border $ C.center $ str "Investment 4",
                    hLimit 25 $ vLimit 5 $  withBorderStyle BS.unicodeRounded $ B.border $ C.center $ str "Investment 5",
                    hLimit 25 $ vLimit 5 $  withBorderStyle BS.unicodeRounded $ B.border $ C.center $ str "Investment 6",
                    hLimit 25 $ vLimit 5 $  withBorderStyle BS.unicodeRounded $ B.border $ C.center $ str "Investment 7"
                    ]
            <=> hBox [ withBorderStyle BS.unicode $ B.border (renderBottomUp (txtWrap <$> loggerContents s)),
                    hLimitPercent 40 (vBox [ withBorderStyle BS.unicodeRounded $ B.border (C.center $ str "current assets"),
                                withBorderStyle BS.unicodeRounded $ B.border ( C.center $ str "pending buy orders"),
                                withBorderStyle BS.unicodeRounded $ B.border ( C.center $ str "pending sell orders")
                                ])
                    ]
            <=> withAttr "statusLine" (padRight Max $ str "[P] Purchase Order [S] Sell Order [Q] Quit [H] Help")

theMap :: AttrMap
theMap = attrMap V.defAttr
    [
        ("statusLine", V.white `on` V.blue),
        ("online", fg V.green),
        ("offline", fg V.red)
    ]

handleEvent :: AppState -> T.BrickEvent () AppEvents -> T.EventM () (T.Next AppState)
handleEvent s (AppEvent (LogEvent l)) = continue $ s { loggerContents = l : loggerContents s }
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

handleStatusEvent :: SystemStatusResponse -> String
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

    sess <- mkSession
    -- authsess <- mkAuthSession

    -- cheap startup routine
    binanceStatus <- systemStatus sess
    writeBChan chan $ StatusEvent binanceStatus
    case binanceStatus of
        Offline s -> logger chan "Binance is offline."
        Maintenance s -> logger chan $ "Binance is down for maintenance." ++ show s
        Online -> do
                    logger chan "Binance is online."
                    binanceExchangeInfo <- exchangeInfo sess

                    case binanceExchangeInfo of
                        Success (Exchangeinfo a b c s) -> logger chan $ "Found " ++ show (length s) ++ " symbols"
                        Failure b -> logger chan "Unable to get binance exchange info"

                    return ()

    -- setUncaughtExceptionHandler ""
    void $ forkIO $ forever $ tickerJob sess chan 10000000
    void $ forkIO $ forever $ statusJob sess chan 1000000

    void $ customMain vty (V.mkVty cfg) (Just chan) theApp initialState

tickerJob :: Session -> BChan AppEvents -> Int -> IO ()
tickerJob sess chan delay = do
    prices <- ticker sess

    case prices of
        Success (p,w) -> do
            writeBChan chan $ TickerEvent $ filter match p
            writeBChan chan $ WeightEvent w
        Failure s  -> logger chan $ "Error fetching ticker prices: " ++ show s

    threadDelay delay
    where
        symbols = ["BTCAUD", "ETHAUD", "BNBAUD", "DOGEAUD", "ADAAUD"]
        match p = tickerSymbol p `elem` symbols

statusJob :: Session -> BChan AppEvents -> Int -> IO ()
statusJob sess chan delay = do
    binanceStatus <- systemStatus sess
    writeBChan chan $ StatusEvent binanceStatus

    case binanceStatus of
            Offline s -> logger chan "Binance is offline."
            Maintenance s -> logger chan $ "Binance is down for maintenance." ++ show s
            Online -> pure ()

    threadDelay delay

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