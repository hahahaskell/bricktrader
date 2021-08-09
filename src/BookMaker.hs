{-# LANGUAGE OverloadedStrings #-}
module BookMaker where

import Lib
import Binance
import Control.Concurrent (MVar)

data OrderState = OrderState { orderCount :: OrderCount
                             , orderCountLimit :: OrderCountLimit
                             , weightCount :: WeightCount
                             , weightCountLimit :: WeigthCountLimit
                             , delta :: Millisecond
                             }

newtype BinanceOrderState = BinanceOrderState (MVar OrderState)
