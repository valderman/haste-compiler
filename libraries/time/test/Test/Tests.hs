module Test.Tests where

import Test.Framework

import Test.AddDays
import Test.ClipDates
import Test.ConvertBack
import Test.LongWeekYears
import Test.TestCalendars
import Test.TestEaster
import Test.TestFormat
import Test.TestMonthDay
import Test.TestParseDAT
import Test.TestParseTime
import Test.TestTime
import Test.TestTimeZone

tests :: [Test]
tests = [ addDaysTest
        , clipDates
        , convertBack
        , longWeekYears
        , testCalendars
        , testEaster
        , testFormat
        , testMonthDay
        , testParseDAT
        , testParseTime
        , testTime
        , testTimeZone ]
