module Util where

import Import
import System.Clock

getWallTime :: TimeSpec -> TimeSpec -> Millisecond
getWallTime start end = diffMsec
  where
    diffNsec = nsec $ end `diffTimeSpec` start
    diffMsec = fromIntegral $ diffNsec `div` 1000000

millisecondToMicrosecond :: Int -> Int
millisecondToMicrosecond milli = milli * 1000
