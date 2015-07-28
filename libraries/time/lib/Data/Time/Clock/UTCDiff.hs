-- #hide
module Data.Time.Clock.UTCDiff where

import Data.Time.Clock.POSIX
import Data.Time.Clock.UTC

-- | addUTCTime a b = a + b
addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
addUTCTime x t = posixSecondsToUTCTime (x + (utcTimeToPOSIXSeconds t))

-- | diffUTCTime a b = a - b
diffUTCTime :: UTCTime -> UTCTime -> NominalDiffTime
diffUTCTime a b = (utcTimeToPOSIXSeconds a) - (utcTimeToPOSIXSeconds b)
