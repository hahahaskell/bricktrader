{-# LANGUAGE OverloadedStrings #-}

module UI (runTui) where

import Brick
import qualified Graphics.Vty as V
import Brick.BChan (newBChan, writeBChan)

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

import Data.Text (Text)
import Data.Map as Map

import Network.Wreq
import Control.Lens
import Data.Aeson (Value, toJSON)
import Data.Aeson.Lens (key, _String)

import Data.Time

import Binance

data AppState = AppState
    { loggerContents :: [T.Text]
    }
data Event = StateUpdated
data AppName = Main

data LoggerEvent = NewLine T.Text


initialState :: AppState
initialState =
    AppState { loggerContents = []
             }

drawUI :: AppState -> [Widget ()]
drawUI s = [a]
    where
        a =
            hBox [str "BTC/AUD $73,000.00 ▼ -0.3%",
                str " | ", str "ETH/AUD $3,0000 ▲ 6.4%",
                str " | ", str "XRP/AUD $1.7000 ▲ 0.6%",
                str " | ", str "LTC/AUD $340.00 ▼ -0.94%",
                str " | ", str "BCH/AUD $1200.00 ▲ 3.62%"
                ]
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
            <=> withAttr "statusLine" (padRight Max $ str "[P] Purchase Order [S] Sell Order [Q] Quit [?] Help")

theMap :: AttrMap
theMap = attrMap V.defAttr
    [
        ("statusLine", V.white `on` V.blue)
    ]

-- handleEvent :: forall e. AppState -> T.BrickEvent () e -> T.EventM () (T.Next AppState)
handleEvent :: AppState -> T.BrickEvent () LoggerEvent -> T.EventM () (T.Next AppState)
handleEvent s (AppEvent (NewLine l        )) = continue $ s { loggerContents = l : loggerContents s }
handleEvent s (VtyEvent (V.EvKey (V.KChar 'Q') [])) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s _ = continue s

theApp :: App AppState LoggerEvent ()
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
        price <- price "BTCAUD"

        writeBChan chan $ NewLine $ "BTCAUD" <> " @ " <> price
        threadDelay 1000000

    void $ customMain vty (V.mkVty cfg) (Just chan) theApp initialState
    -- void $ M.defaultMain app initialState

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