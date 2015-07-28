{-# OPTIONS -fno-warn-unused-imports #-}
#include "HsConfigure.h"
-- | TAI and leap-second tables for converting to UTC: most people won't need this module.
module Data.Time.Clock.TAI
(
	-- TAI arithmetic
	AbsoluteTime,taiEpoch,addAbsoluteTime,diffAbsoluteTime,

	-- leap-second table type
	LeapSecondTable,

	-- conversion between UTC and TAI with table
	utcDayLength,utcToTAITime,taiToUTCTime,

	parseTAIUTCDATFile
) where

import Data.Time.LocalTime
import Data.Time.Calendar.Days
import Data.Time.Clock
import Control.DeepSeq
import Data.Typeable
import Data.Fixed
#if LANGUAGE_Rank2Types
import Data.Data
#endif

-- | AbsoluteTime is TAI, time as measured by a clock.
newtype AbsoluteTime = MkAbsoluteTime {unAbsoluteTime :: DiffTime} deriving (Eq,Ord
#if LANGUAGE_DeriveDataTypeable
#if LANGUAGE_Rank2Types
#if HAS_DataPico
    ,Data, Typeable
#endif
#endif
#endif
    )

instance NFData AbsoluteTime where
	rnf (MkAbsoluteTime a) = rnf a

instance Show AbsoluteTime where
	show t = show (utcToLocalTime utc (taiToUTCTime (const 0) t)) ++ " TAI" -- ugly, but standard apparently

-- | The epoch of TAI, which is 1858-11-17 00:00:00 TAI.
taiEpoch :: AbsoluteTime
taiEpoch = MkAbsoluteTime 0

-- | addAbsoluteTime a b = a + b
addAbsoluteTime :: DiffTime -> AbsoluteTime -> AbsoluteTime
addAbsoluteTime t (MkAbsoluteTime a) = MkAbsoluteTime (a + t)

-- | diffAbsoluteTime a b = a - b
diffAbsoluteTime :: AbsoluteTime -> AbsoluteTime -> DiffTime
diffAbsoluteTime (MkAbsoluteTime a) (MkAbsoluteTime b) = a - b

-- | TAI - UTC during this day.
-- No table is provided, as any program compiled with it would become
-- out of date in six months.
type LeapSecondTable = Day -> Integer

utcDayLength :: LeapSecondTable -> Day -> DiffTime
utcDayLength table day = realToFrac (86400 + (table (addDays 1 day)) - (table day))

dayStart :: LeapSecondTable -> Day -> AbsoluteTime
dayStart table day = MkAbsoluteTime	(realToFrac ((toModifiedJulianDay day) * 86400 + (table day)))

utcToTAITime :: LeapSecondTable -> UTCTime -> AbsoluteTime
utcToTAITime table (UTCTime day dtime) = MkAbsoluteTime (t + dtime) where
	MkAbsoluteTime t = dayStart table day

taiToUTCTime :: LeapSecondTable -> AbsoluteTime -> UTCTime
taiToUTCTime table abstime = stable (ModifiedJulianDay (div' (unAbsoluteTime abstime) 86400)) where
	stable day = if (day == day') then UTCTime day dtime else stable day' where
		dayt = dayStart table day
		dtime = diffAbsoluteTime abstime dayt
		day' = addDays (div' dtime (utcDayLength table day)) day

-- | Parse the contents of a tai-utc.dat file.
-- This does not do any kind of validation and will return a bad table for input
-- not in the correct format.
parseTAIUTCDATFile :: String -> LeapSecondTable
parseTAIUTCDATFile ss = offsetlist 0 (parse (lines ss)) where
	offsetlist :: Integer -> [(Day,Integer)] -> LeapSecondTable
	offsetlist i [] _ = i
	offsetlist i ((d0,_):_) d | d < d0 = i
	offsetlist _ ((_,i0):xx) d = offsetlist i0 xx d
	
	parse :: [String] -> [(Day,Integer)]
	parse [] = []
	parse (a:as) = let
		ps = parse as
	 in case matchLine a of
		Just di -> di:ps
		Nothing -> ps
	
	matchLine :: String -> Maybe (Day,Integer)
	matchLine s = do
		check0S s
		(d,s') <- findJD s
		i <- findOffset s'
		return (d,i)
	
	-- a bit fragile
	check0S :: String -> Maybe ()
	check0S "X 0.0      S" = Just ()
	check0S [] = Nothing
	check0S (_:cs) = check0S cs
	
	findJD :: String -> Maybe (Day,String)
	findJD ('=':'J':'D':s) = do
		d <- getInteger '5' s
		return (ModifiedJulianDay (d - 2400000),s)
	findJD [] = Nothing
	findJD (_:cs) = findJD cs
	
	findOffset :: String -> Maybe Integer
	findOffset ('T':'A':'I':'-':'U':'T':'C':'=':s) = getInteger '0' s
	findOffset [] = Nothing
	findOffset (_:cs) = findOffset cs
	
	getInteger :: Char -> String -> Maybe Integer
	getInteger p s = do
		digits <- getDigits p s
		fromDigits 0 digits
	
	getDigits :: Char -> String -> Maybe String
	getDigits p (' ':s) = getDigits p s
	getDigits p (c:cs) | c >= '0' && c <= '9' = do
		s <- getDigits p cs
		return (c:s)
	getDigits p ('.':p1:_) = if p == p1 then Just [] else Nothing
	getDigits _ _ = Nothing
		
	
	fromDigits :: Integer -> String -> Maybe Integer
	fromDigits i [] = Just i
	fromDigits i (c:cs) | c >= '0' && c <= '9' = fromDigits ((i * 10) + (fromIntegral ((fromEnum c) - (fromEnum '0')))) cs
	fromDigits _ _ = Nothing

-- typical line format:
-- 1972 JAN  1 =JD 2441317.5  TAI-UTC=  10.0       S + (MJD - 41317.) X 0.0      S
-- 1972 JUL  1 =JD 2441499.5  TAI-UTC=  11.0       S + (MJD - 41317.) X 0.0      S
