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

import Data.Text (Text, unpack, pack)
import Data.Time

import Binance
import Control.Monad.IO.Class (liftIO)

data AppState = AppState
    { loggerContents :: [Text]
    , tickerContents :: String
    }
data AppName = Main

data AppEvents =
     LogEvent Text
     | PriceEvent [PriceResponse]

initialState :: AppState
initialState =
    AppState { loggerContents = ["Welcome to BrickTrader."]
             , tickerContents = "..."
             }

drawUI :: AppState -> [Widget ()]
drawUI s = [a]
    where
        a =
            hBox [str $ tickerContents s] -- "BTC/AUD $73,000.00 ▼ -0.3%"
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
        ("statusLine", V.white `on` V.blue)
    ]

handleEvent :: AppState -> T.BrickEvent () AppEvents -> T.EventM () (T.Next AppState)
handleEvent s (AppEvent (LogEvent l)) = continue $ s { loggerContents = l : loggerContents s }
handleEvent s (AppEvent (PriceEvent p)) = continue $ s { tickerContents = handleTicker p }
handleEvent s (VtyEvent (V.EvKey (V.KChar 'Q') [])) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s _ = continue s

handleTicker :: [PriceResponse] -> String
handleTicker pr = "| " ++ foldMap format pr
    where
        format (PriceResponse s p) = unpack s ++ " $" ++ unpack p ++ " | "

theApp :: App AppState AppEvents ()
theApp = M.App { appDraw         = drawUI
               , appChooseCursor = neverShowCursor
               , appHandleEvent  = handleEvent
               , appAttrMap      = const theMap
               , appStartEvent   = return
               }

runTui :: IO ()
runTui = do
    cfg <- V.standardIOConfig
    vty <- V.mkVty cfg
    chan <- newBChan 10

    void $ forkIO $ forever $ do
        let symbols = ["BTCAUD", "ETHAUD", "XRPAUD", "BNBAUD", "DOGEAUD", "ADAAUD"]

        prices <- prices symbols
        writeBChan chan $ PriceEvent prices

        _ <- logger chan "testtingg"

        threadDelay 1000000

    void $ customMain vty (V.mkVty cfg) (Just chan) theApp initialState

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