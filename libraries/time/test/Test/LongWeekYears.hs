module Test.LongWeekYears where

import Data.Time.Calendar.WeekDate
import Data.Time.Calendar
import Test.TestUtil
import Test.LongWeekYearsRef

longYear :: Integer -> Bool
longYear year = case toWeekDate (fromGregorian year 12 31) of
	(_,53,_) -> True
	_ -> False

showLongYear :: Integer -> String
showLongYear year
  = unwords [ show year ++ ":"
            , (if isLeapYear year then "L" else " ") ++ (if longYear year then "*" else " ") ]

longWeekYears :: Test
longWeekYears = pureTest "longWeekYears" $
    diff longWeekYearsRef $ unlines $ map showLongYear [1901 .. 2050]
