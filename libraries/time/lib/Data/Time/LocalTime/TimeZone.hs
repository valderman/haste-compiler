{-# OPTIONS -fno-warn-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#ifdef __HASTE__
{-# LANGUAGE OverloadedStrings #-}
#endif
#include "HsConfigure.h"

-- #hide
module Data.Time.LocalTime.TimeZone
(
	-- * Time zones
	TimeZone(..),timeZoneOffsetString,timeZoneOffsetString',minutesToTimeZone,hoursToTimeZone,utc,

	-- getting the locale time zone
	getTimeZone,getCurrentTimeZone
) where

--import System.Time.Calendar.Format
import Data.Time.Calendar.Private
import Data.Time.Clock
import Data.Time.Clock.POSIX

#if __GLASGOW_HASKELL__ >= 709
import Foreign
#else
import Foreign.Safe
#endif
import Foreign.C
import Control.DeepSeq
import Data.Typeable
#if LANGUAGE_Rank2Types
import Data.Data
#endif
#ifdef __HASTE__
import Data.Time.Calendar (toGregorian)
import Haste.Prim.JSType (toString)
import Haste.Prim.Foreign
#endif

-- | A TimeZone is a whole number of minutes offset from UTC, together with a name and a \"just for summer\" flag.
data TimeZone = TimeZone {
	-- | The number of minutes offset from UTC. Positive means local time will be later in the day than UTC.
	timeZoneMinutes :: Int,
	-- | Is this time zone just persisting for the summer?
	timeZoneSummerOnly :: Bool,
	-- | The name of the zone, typically a three- or four-letter acronym.
	timeZoneName :: String
} deriving (Eq,Ord
#if LANGUAGE_DeriveDataTypeable
#if LANGUAGE_Rank2Types
    ,Data, Typeable
#endif
#endif
    )

instance NFData TimeZone where
	rnf (TimeZone m so n) = m `deepseq` so `deepseq` n `deepseq` ()

-- | Create a nameless non-summer timezone for this number of minutes
minutesToTimeZone :: Int -> TimeZone
minutesToTimeZone m = TimeZone m False ""

-- | Create a nameless non-summer timezone for this number of hours
hoursToTimeZone :: Int -> TimeZone
hoursToTimeZone i = minutesToTimeZone (60 * i)

showT :: NumericPadOption -> Int -> String
showT opt t = show4 opt ((div t 60) * 100 + (mod t 60))

-- | Text representing the offset of this timezone, such as \"-0800\" or \"+0400\" (like %z in formatTime), with arbitrary padding
timeZoneOffsetString' :: NumericPadOption -> TimeZone -> String
timeZoneOffsetString' opt (TimeZone t _ _) | t < 0 = '-':(showT opt (negate t))
timeZoneOffsetString' opt (TimeZone t _ _) = '+':(showT opt t)

-- | Text representing the offset of this timezone, such as \"-0800\" or \"+0400\" (like %z in formatTime)
timeZoneOffsetString :: TimeZone -> String
timeZoneOffsetString = timeZoneOffsetString' (Just '0')

instance Show TimeZone where
	show zone@(TimeZone _ _ "") = timeZoneOffsetString zone
	show (TimeZone _ _ name) = name

-- | The UTC time zone
utc :: TimeZone
utc = TimeZone 0 False "UTC"

#ifdef __HASTE__
tzOffMins :: Int -> Int -> Int -> IO Int
tzOffMins = ffi "(function(y,m,d){return new Date(y,m,d).getTimezoneOffset();})"

tzIsSummerOnly :: IO Bool
tzIsSummerOnly = ffi "(function(){\
var d = new Date();\
d.setMonth(11);\
var off = d.getTimezoneOffset();\
d.setMonth(5);\
return off != d.getTimezoneOffset();\
})"

-- | Get the local time-zone for a given time (varying as per summertime adjustments)
getTimeZone :: UTCTime -> IO TimeZone
getTimeZone (UTCTime t _) = do
    off <- tzOffMins (fromInteger y) m d
    summer <- tzIsSummerOnly
    let (hs, ms) = abs off `quotRem` 60
        utcoff = if ms == 0
                   then toString hs
                   else toString hs ++ ":" ++ toString ms
        name =
          case True of
            _ | off < 0   -> "GMT+" ++ utcoff
              | off > 0   -> "GMT-" ++ utcoff
              | otherwise -> "GMT"
    return $ TimeZone off summer name
  where
    (y, m, d) = toGregorian t

#else

posixToCTime :: POSIXTime -> CTime
posixToCTime  = fromInteger . floor

{-# CFILES cbits/HsTime.c #-}
foreign import ccall unsafe "HsTime.h get_current_timezone_seconds" get_current_timezone_seconds :: CTime -> Ptr CInt -> Ptr CString -> IO CLong

-- | Get the local time-zone for a given time (varying as per summertime adjustments)
getTimeZone :: UTCTime -> IO TimeZone
getTimeZone time = with 0 (\pdst -> with nullPtr (\pcname -> do
	secs <- get_current_timezone_seconds (posixToCTime (utcTimeToPOSIXSeconds time)) pdst pcname
	case secs of
		0x80000000 -> fail "localtime_r failed"
		_ -> do
			dst <- peek pdst
			cname <- peek pcname
			name <- peekCString cname
			return (TimeZone (div (fromIntegral secs) 60) (dst == 1) name)
	))
#endif

-- | Get the current time-zone
getCurrentTimeZone :: IO TimeZone
getCurrentTimeZone = getCurrentTime >>= getTimeZone
