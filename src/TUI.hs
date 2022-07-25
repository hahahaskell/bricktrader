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
    ( attrMap,
      continue,
      halt,
      attrL,
      getContext,
      handleEventLensed,
      suffixLenses,
      on,
      (<=>),
      fill,
      hBox,
      hLimitPercent,
      padLeft,
      padLeftRight,
      padRight,
      raw,
      str,
      txtWrap,
      vBox,
      vLimitPercent,
      withAttr,
      withBorderStyle,
      AttrMap,
      Padding(Max, Pad),
      Size(Greedy),
      Widget(Widget, render),
      BrickEvent(VtyEvent, AppEvent),
      Context(availHeight),
      Result(image) )
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
import Control.Lens ((^.), view, set)
import Data.Text (unpack)

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
    , contentSelectedSymbol :: Text
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
            hBox [ withAttr "headerbarAttr" $ str "BrickTrader" --,  str $ unpack s.contentSelectedSymbol
                 , withAttr "headerbarAttr" $ padLeft Max $ hBox [
                                      padLeftRight 1 (str "[") , str (contentLatency s) , padLeft (Pad 1) (str "]")
                                     , padLeftRight 1 (str "[") , str (contentTimeDelta s) , padLeft (Pad 1) (str "]")
                                     , padLeftRight 1 (str "[") , str (contentWeightLimit s) , padLeft (Pad 1) (str "]")
                                     , padLeftRight 1 (str "[") , str (contentStatus s) , padLeft (Pad 1) (str "]")
                                     ]
                ]
            <=> hBox [ str $ contentTicker s ]
            <=> hBox [  vBox [ withBorderStyle BS.unicode $ B.border (renderGraph (txtWrap <$> [s.contentSelectedSymbol]))
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
        drawListElement _sel a = str $ prettyPrint a
                                -- if sel
                                -- -- then withAttr "headerbarAttr" $ str $ prettyPrint a
                                -- else str $ prettyPrint a
                where
                    prettyPrint = filter Char.isLetter . show

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
handleEvent s (VtyEvent (V.EvKey (V.KEnter) [])) = handleListSelectionEvent s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'Q') [])) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s (VtyEvent e) = continue =<< handleEventLensed s contentSymbolsListL (L.handleListEvent) e
handleEvent s _ = continue s

-- new <- pure $ L.listInsert 0 ("b"::Text) (L.list () (Vec.fromList ["a"]) 1)

-- handleTickerEvent :: [Ticker] -> String
-- handleTickerEvent pr = "| " ++ foldMap format pr
--     where
--         format t = printf "%s $%.2f %c %.2f%% | " (symbol t) (lastPrice t) (arrow $ percent t) (percent t)
--         symbol t = unpack $ tickerSymbol t
--         percent t = read $ unpack (tickerPriceChangePercent t) :: Double
--         lastPrice t =  read $ unpack (tickerLastPrice t) :: Double
--         arrow p = if p < 0 then '⯆' else '⯅'

handleListSelectionEvent :: AppContent -> T.EventM n (T.Next AppContent)
handleListSelectionEvent s = case L.listSelectedElement s.contentSymbolsList of
                                Nothing -> continue s
                                Just (_, e) -> continue $ s { contentSelectedSymbol = e }

handleStatusEvent :: SystemStatus -> String
handleStatusEvent Online = "Online" -- "🟢"
handleStatusEvent (Offline _) =  "Offline" --"🔴"
handleStatusEvent (Maintenance _) = "Down" -- "🚧"

handleWeightEvent :: WeightCount -> String
handleWeightEvent w =  show w ++ "/1200"

handleLatencyEvent :: Millisecond -> String
handleLatencyEvent m = show m ++ "ms"

renderGraph :: [Widget n] -> Widget n
renderGraph ws =
    Widget Greedy Greedy $ do
        ctx <- getContext

        let a = ctx^.attrL
        render $ ws !! 0
        -- render $ fill 'r'

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