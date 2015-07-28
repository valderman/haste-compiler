module Test.TestParseDAT where

import Data.Time
import Data.Time.Clock.TAI
import Test.TestUtil
import Test.TestParseDAT_Ref
import Test.TAI_UTC_DAT

tods :: [TimeOfDay]
tods = [
	TimeOfDay 0 0 0,
	TimeOfDay 0 0 0.5,
	TimeOfDay 0 0 1,
	TimeOfDay 0 0 1.5,
	TimeOfDay 0 0 2,
	TimeOfDay 23 59 28,
	TimeOfDay 23 59 28.5,
	TimeOfDay 23 59 29,
	TimeOfDay 23 59 29.5,
	TimeOfDay 23 59 30,
	TimeOfDay 23 59 30.5,
	TimeOfDay 23 59 31,
	TimeOfDay 23 59 31.5,
	TimeOfDay 23 59 32,
	TimeOfDay 23 59 59,
	TimeOfDay 23 59 59.5,
	TimeOfDay 23 59 60,
	TimeOfDay 23 59 60.5
	]

times :: [LocalTime]
times =
	fmap (LocalTime (fromGregorian 1998 04 02)) tods ++
	fmap (LocalTime (fromGregorian 1998 12 30)) tods ++
	fmap (LocalTime (fromGregorian 1998 12 31)) tods ++
	fmap (LocalTime (fromGregorian 1999 01 01)) tods ++
	fmap (LocalTime (fromGregorian 1999 01 02)) tods

testParseDAT :: Test
testParseDAT = pureTest "testParseDAT" $ diff testParseDAT_Ref parseDAT where
    parseDAT = 
        let lst = parseTAIUTCDATFile taiUTC_DAT in 
        unlines $ map
        (\lt ->
            let
                utcTime  = localTimeToUTC utc lt
                taiTime  = utcToTAITime lst utcTime
                utcTime' = taiToUTCTime lst taiTime
            in if utcTime == utcTime'
                then unwords [show utcTime, "==", show taiTime]
                else unwords [ "correction:", show utcTime, "->", show taiTime, "->", show utcTime']
        )
        times
