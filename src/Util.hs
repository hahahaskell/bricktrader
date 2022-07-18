module Util where

import Types
import System.Clock
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as Http
import Control.Lens ((^.))
import Network.Wreq (responseHeader)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

getWallTime :: TimeSpec -> TimeSpec -> Millisecond
getWallTime start end = diffMsec
  where
    diffNsec = nsec $ end `diffTimeSpec` start
    diffMsec = fromIntegral $ diffNsec `div` 1000000

millisecondToMicrosecond :: Int -> Int
millisecondToMicrosecond milli = milli * 1000

getHeader :: Http.Response body -> Http.HeaderName -> String
getHeader res name = show $ res ^. responseHeader name

utcTimeToEpochTime :: UTCTime -> EpochTime
utcTimeToEpochTime t = round $ utcTimeToPOSIXSeconds t
