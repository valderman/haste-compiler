module Test.ConvertBack where

import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.Julian
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar
import Test.TestUtil

checkDay :: (Show t) => (Day -> t) -> (t -> Day) -> (t -> Maybe Day) -> Day -> String
checkDay encodeDay decodeDay decodeDayValid day
  = let st    = encodeDay day
	day'  = decodeDay st
	mday' = decodeDayValid st

	a = if day /= day'
	      then unwords [ show day, "-> "
                           , show st,  "-> "
                           , show day'
                           , "(diff", show (diffDays day' day) ++ ")" ]
	      else ""

	b = if Just day /= mday'
	      then unwords [show day, "->", show st, "->", show mday']
	      else ""
    in a ++ b
		
checkers :: [Day -> String]
checkers
  = [ checkDay toOrdinalDate (\(y,d) -> fromOrdinalDate y d) (\(y,d) -> fromOrdinalDateValid y d)
    , checkDay toWeekDate (\(y,w,d) -> fromWeekDate y w d) (\(y,w,d) -> fromWeekDateValid y w d)
    , checkDay toGregorian (\(y,m,d) -> fromGregorian y m d) (\(y,m,d) -> fromGregorianValid y m d)
    , checkDay toJulian (\(y,m,d) -> fromJulian y m d) (\(y,m,d) -> fromJulianValid y m d) ]

days :: [Day]
days = [ModifiedJulianDay 50000 .. ModifiedJulianDay 50200] ++
	(fmap (\year -> (fromGregorian year 1 4)) [1980..2000])

convertBack :: Test
convertBack = pureTest "convertBack" $
    diff "" $ concatMap (\ch -> concatMap ch days) checkers
