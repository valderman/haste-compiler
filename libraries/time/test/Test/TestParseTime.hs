{-# OPTIONS -fno-warn-type-defaults -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, ExistentialQuantification #-}

module Test.TestParseTime where

import Control.Monad
import Data.Char
import Data.Ratio
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.Clock.POSIX
import Test.QuickCheck hiding (Result,reason)
import Test.QuickCheck.Property hiding (result)
import Test.TestUtil hiding (Result)

ntest :: Int
ntest = 1000

type NamedProperty = (String, Property)

testParseTime :: Test
testParseTime = testGroup "testParseTime"
    [
    readOtherTypesTest,
    readTests,
    simpleFormatTests,
    extests,
    particularParseTests,
    badParseTests,
    defaultTimeZoneTests,
    militaryTimeZoneTests,
    testGroup "properties" (fmap (\(n,prop) -> testProperty n prop) properties)
    ]

yearDays :: Integer -> [Day]
yearDays y = [(fromGregorian y 1 1) .. (fromGregorian y 12 31)]

makeExhaustiveTest :: String -> [t] -> (t -> Test) -> Test
makeExhaustiveTest name cases f = testGroup name (fmap f cases)

extests :: Test
extests = testGroup "exhaustive" ([
    makeExhaustiveTest "parse %y" [0..99] parseYY,
    makeExhaustiveTest "parse %-C %y 1900s" [0,1,50,99] (parseCYY 19),
    makeExhaustiveTest "parse %-C %y 2000s" [0,1,50,99] (parseCYY 20),
    makeExhaustiveTest "parse %-C %y 1400s" [0,1,50,99] (parseCYY 14),
    makeExhaustiveTest "parse %C %y 0700s" [0,1,50,99] (parseCYY2 7),
    makeExhaustiveTest "parse %-C %y 700s" [0,1,50,99] (parseCYY 7),
    makeExhaustiveTest "parse %-C %y 10000s" [0,1,50,99] (parseCYY 100),
    makeExhaustiveTest "parse %-C centuries" [20..100] (parseCentury " "),
    makeExhaustiveTest "parse %-C century X" [1,10,20,100] (parseCentury "X"),
    makeExhaustiveTest "parse %-C century 2sp" [1,10,20,100] (parseCentury "  "),
    makeExhaustiveTest "parse %-C century 5sp" [1,10,20,100] (parseCentury "     ")
    ] ++
    (concat $ fmap
    (\y -> [
    (makeExhaustiveTest "parse %Y%m%d" (yearDays y) parseYMD),
    (makeExhaustiveTest "parse %Y %m %d" (yearDays y) parseYearDayD),
    (makeExhaustiveTest "parse %Y %-m %e" (yearDays y) parseYearDayE)
    ]) [1,4,20,753,2000,2011,10001]))

readTest :: (Eq a,Show a,Read a) => [(a,String)] -> String -> Test
readTest expected target = let
    found = reads target
    result = diff expected found
    name = show target
    in pureTest name result

readTestsParensSpaces :: forall a. (Eq a,Show a,Read a) => a -> String -> Test
readTestsParensSpaces expected target = testGroup target
    [
    readTest [(expected,"")] $ target,
    readTest [(expected,"")] $ "("++target++")",
    readTest [(expected,"")] $ " ("++target++")",
    readTest [(expected," ")] $ " ( "++target++" ) ",
    readTest [(expected," ")] $ " (( "++target++" )) ",
    readTest ([] :: [(a,String)]) $ "("++target,
    readTest [(expected,")")] $ ""++target++")",
    readTest [(expected,"")] $ "(("++target++"))",
    readTest [(expected," ")] $ "  (   (     "++target++"   )  ) "
    ] where

readOtherTypesTest :: Test
readOtherTypesTest = testGroup "read other types"
    [
    readTestsParensSpaces 3 "3",
    readTestsParensSpaces "a" "\"a\""
    ]

readTests :: Test
readTests = testGroup "read times"
    [
    readTestsParensSpaces testDay "1912-07-08",
    --readTestsParensSpaces testDay "1912-7-8",
    readTestsParensSpaces testTimeOfDay "08:04:02"
    --,readTestsParensSpaces testTimeOfDay "8:4:2"
    ] where
    testDay = fromGregorian 1912 7 8
    testTimeOfDay = TimeOfDay 8 4 2

epoch :: LocalTime
epoch = LocalTime (fromGregorian 1970 0 0) midnight

simpleFormatTests :: Test
simpleFormatTests = testGroup "simple"
    [
    readsTest [(epoch,"")] "" "",
    readsTest [(epoch," ")] "" " ",
    readsTest [(epoch,"")] " " " ",
    readsTest [(epoch,"")] " " "  ",
    readsTest [(epoch,"")] "%k" "0",
    readsTest [(epoch,"")] "%k" " 0",
    readsTest [(epoch,"")] "%m" "01",
    readsTest [(epoch," ")] "%m" "01 ",
    readsTest [(epoch," ")] " %m" " 01 ",
    readsTest [(epoch,"")] " %m" " 01",
    -- https://ghc.haskell.org/trac/ghc/ticket/9150
    readsTest [(epoch,"")] " %M" " 00",
    readsTest [(epoch,"")] "%M " "00 ",
    readsTest [(epoch,"")] "%Q" "",
    readsTest [(epoch," ")] "%Q" " ",
    readsTest [(epoch,"X")] "%Q" "X",
    readsTest [(epoch," X")] "%Q" " X",
    readsTest [(epoch,"")] "%Q " " ",
    readsTest [(epoch,"")] "%Q X" " X",
    readsTest [(epoch,"")] "%QX" "X"
    ] where
    readsTest :: (Show a, Eq a, ParseTime a) => [(a,String)] -> String -> String -> Test
    readsTest expected formatStr target = let
        found = readSTime False defaultTimeLocale formatStr target
        result = diff expected found
        name = (show formatStr) ++ " of " ++ (show target)
        in pureTest name result

spacingTests :: (Show t, Eq t, ParseTime t) => t -> String -> String -> Test
spacingTests expected formatStr target = testGroup "particular"
    [
        parseTest False (Just expected) formatStr target,
        parseTest True (Just expected) formatStr target,
        parseTest False (Just expected) (formatStr ++ " ") (target ++ " "),
        parseTest True (Just expected) (formatStr ++ " ") (target ++ " "),
        parseTest False (Just expected) (" " ++ formatStr) (" " ++ target),
        parseTest True (Just expected) (" " ++ formatStr) (" " ++ target),
        parseTest True (Just expected) ("" ++ formatStr) (" " ++ target),
        parseTest True (Just expected) (" " ++ formatStr) ("  " ++ target)
    ]

particularParseTests :: Test
particularParseTests = testGroup "particular"
    [
        spacingTests epoch "%Q" "",
        spacingTests epoch "%Q" ".0",
        spacingTests epoch "%k" " 0",
        spacingTests epoch "%M" "00",
        spacingTests epoch "%m" "01",
        spacingTests (TimeZone 120 False "") "%z" "+0200",
        spacingTests (TimeZone 120 False "") "%Z" "+0200",
        spacingTests (TimeZone (-480) False "PST") "%Z" "PST"
    ]

badParseTests :: Test
badParseTests = testGroup "bad"
    [
        parseTest False (Nothing :: Maybe Day) "%Y" ""
    ]

parseYMD :: Day -> Test
parseYMD day = case toGregorian day of
    (y,m,d) -> parseTest False (Just day) "%Y%m%d" ((show y) ++ (show2 m) ++ (show2 d))

parseYearDayD :: Day -> Test
parseYearDayD day = case toGregorian day of
    (y,m,d) -> parseTest False (Just day) "%Y %m %d" ((show y) ++ " " ++ (show2 m) ++ " " ++ (show2 d))

parseYearDayE :: Day -> Test
parseYearDayE day = case toGregorian day of
    (y,m,d) -> parseTest False (Just day) "%Y %-m %e" ((show y) ++ " " ++ (show m) ++ " " ++ (show d))

-- | 1969 - 2068
expectedYear :: Integer -> Integer
expectedYear i | i >= 69 = 1900 + i
expectedYear i = 2000 + i

show2 :: (Show n,Integral n) => n -> String
show2 i = (show (div i 10)) ++ (show (mod i 10))

parseYY :: Integer -> Test
parseYY i = parseTest False (Just (fromGregorian (expectedYear i) 1 1)) "%y" (show2 i)

parseCYY :: Integer -> Integer -> Test
parseCYY c i = parseTest False (Just (fromGregorian ((c * 100) + i) 1 1)) "%-C %y" ((show c) ++ " " ++ (show2 i))

parseCYY2 :: Integer -> Integer -> Test
parseCYY2 c i = parseTest False (Just (fromGregorian ((c * 100) + i) 1 1)) "%C %y" ((show2 c) ++ " " ++ (show2 i))

parseCentury :: String -> Integer -> Test
parseCentury int c = parseTest False (Just (fromGregorian (c * 100) 1 1)) ("%-C" ++ int ++ "%y") ((show c) ++ int ++ "00")

parseTest :: (Show t, Eq t, ParseTime t) => Bool -> Maybe t -> String -> String -> Test
parseTest sp expected formatStr target =
    let
        found = parse sp formatStr target
        result = diff expected found
        name = (show formatStr) ++ " of " ++ (show target) ++ (if sp then " allowing spaces" else "")
    in pureTest name result
{-
readsTest :: forall t. (Show t, Eq t, ParseTime t) => Maybe t -> String -> String -> Test
readsTest (Just e) = readsTest' [(e,"")]
readsTest Nothing = readsTest' ([] :: [(t,String)])
-}

enumAdd :: (Enum a) => Int -> a -> a
enumAdd i a = toEnum (i + fromEnum a)

getMilZoneLetter :: Int -> Char
getMilZoneLetter 0 = 'Z'
getMilZoneLetter h | h < 0 = enumAdd (negate h) 'M'
getMilZoneLetter h | h < 10 = enumAdd (h - 1) 'A'
getMilZoneLetter h = enumAdd (h - 10) 'K'

getMilZone :: Int -> TimeZone
getMilZone hour = TimeZone (hour * 60) False [getMilZoneLetter hour]

testParseTimeZone :: TimeZone -> Test
testParseTimeZone tz = parseTest False (Just tz) "%Z" (timeZoneName tz)

defaultTimeZoneTests :: Test
defaultTimeZoneTests = testGroup "default time zones" (fmap testParseTimeZone (knownTimeZones defaultTimeLocale))

militaryTimeZoneTests :: Test
militaryTimeZoneTests = testGroup "military time zones" (fmap (testParseTimeZone . getMilZone) [-12 .. 12])


parse :: ParseTime t => Bool -> String -> String -> Maybe t
parse sp f t = parseTimeM sp defaultTimeLocale f t

format :: (FormatTime t) => String -> t -> String
format f t = formatTime defaultTimeLocale f t

instance Arbitrary Day where
    arbitrary = liftM ModifiedJulianDay $ choose (-313698, 2973483) -- 1000-01-1 to 9999-12-31

instance CoArbitrary Day where
    coarbitrary (ModifiedJulianDay d) = coarbitrary d

instance Arbitrary DiffTime where
    arbitrary = oneof [intSecs, fracSecs] -- up to 1 leap second
        where intSecs = liftM secondsToDiffTime' $ choose (0, 86400)
              fracSecs = liftM picosecondsToDiffTime' $ choose (0, 86400 * 10^12)
              secondsToDiffTime' :: Integer -> DiffTime
              secondsToDiffTime' = fromInteger
              picosecondsToDiffTime' :: Integer -> DiffTime
              picosecondsToDiffTime' x = fromRational (x % 10^12)

instance CoArbitrary DiffTime where
    coarbitrary t = coarbitrary (fromEnum t)

instance Arbitrary TimeOfDay where
    arbitrary = liftM timeToTimeOfDay arbitrary

instance CoArbitrary TimeOfDay where
    coarbitrary t = coarbitrary (timeOfDayToTime t)

instance Arbitrary LocalTime where
    arbitrary = liftM2 LocalTime arbitrary arbitrary

instance CoArbitrary LocalTime where
    coarbitrary t = coarbitrary (truncate (utcTimeToPOSIXSeconds (localTimeToUTC utc t)) :: Integer)

instance Arbitrary TimeZone where
    arbitrary = liftM minutesToTimeZone $ choose (-720,720)

instance CoArbitrary TimeZone where
    coarbitrary tz = coarbitrary (timeZoneMinutes tz)

instance Arbitrary ZonedTime where
    arbitrary = liftM2 ZonedTime arbitrary arbitrary

instance CoArbitrary ZonedTime where
    coarbitrary t = coarbitrary (truncate (utcTimeToPOSIXSeconds (zonedTimeToUTC t)) :: Integer)

instance Arbitrary UTCTime where
    arbitrary = liftM2 UTCTime arbitrary arbitrary

instance CoArbitrary UTCTime where
    coarbitrary t = coarbitrary (truncate (utcTimeToPOSIXSeconds t) :: Integer)

-- missing from the time package
instance Eq ZonedTime where
    ZonedTime t1 tz1 == ZonedTime t2 tz2 = t1 == t2 && tz1 == tz2

compareResult' :: (Eq a,Show a) => String -> a -> a -> Result
compareResult' extra expected found
    | expected == found = succeeded
    | otherwise = failed {reason = "expected " ++ (show expected) ++ ", found " ++ (show found) ++ extra}

compareResult :: (Eq a,Show a) => a -> a -> Result
compareResult = compareResult' ""

compareParse :: forall a. (Eq a,Show a,ParseTime a) => a -> String -> String -> Result
compareParse expected fmt text = compareResult' (", parsing " ++ (show text)) (Just expected) (parse False fmt text)

--
-- * tests for dbugging failing cases
--

test_parse_format :: (FormatTime t,ParseTime t,Show t) => String -> t -> (String,String,Maybe t)
test_parse_format f t = let s = format f t in (show t, s, parse False f s `asTypeOf` Just t)

--
-- * show and read
--

prop_read_show :: (Read a, Show a, Eq a) => a -> Result
prop_read_show t = compareResult [(t,"")] (reads (show t))

prop_read_show' :: (Read a, Show a, Eq a) => a -> Result
prop_read_show' t = compareResult t (read (show t))

--
-- * special show functions
--

prop_parse_showWeekDate :: Day -> Result
prop_parse_showWeekDate d = compareParse d "%G-W%V-%u" (showWeekDate d)

prop_parse_showGregorian :: Day -> Result
prop_parse_showGregorian d = compareParse d "%Y-%m-%d" (showGregorian d)

prop_parse_showOrdinalDate :: Day -> Result
prop_parse_showOrdinalDate d = compareParse d "%Y-%j" (showOrdinalDate d)

--
-- * fromMondayStartWeek and fromSundayStartWeek
--

prop_fromMondayStartWeek :: Day -> Result
prop_fromMondayStartWeek d =
    let (w,wd)  = mondayStartWeek d
        (y,_,_) = toGregorian d
     in compareResult d (fromMondayStartWeek y w wd)

prop_fromSundayStartWeek :: Day -> Result
prop_fromSundayStartWeek d =
    let (w,wd)  = sundayStartWeek d
        (y,_,_) = toGregorian d
     in compareResult d (fromSundayStartWeek y w wd)

--
-- * format and parse
--

-- | Helper for defining named properties.
prop_named :: (Arbitrary t, Show t, Testable a)
           => String -> (FormatString s -> t -> a) -> String -> FormatString s -> NamedProperty
prop_named n prop typeName f = (n ++ " " ++ typeName ++ " " ++ show f, property (prop f))

prop_parse_format :: (Eq t, FormatTime t, ParseTime t, Show t) => FormatString t -> t -> Result
prop_parse_format (FormatString f) t = compareParse t f (format f t)

prop_parse_format_named :: (Arbitrary t, Eq t, Show t, FormatTime t, ParseTime t)
                           => String -> FormatString t -> NamedProperty
prop_parse_format_named = prop_named "prop_parse_format" prop_parse_format

-- Verify case-insensitivity with upper case.
prop_parse_format_upper :: (Eq t, FormatTime t, ParseTime t, Show t) => FormatString t -> t -> Result
prop_parse_format_upper (FormatString f) t = compareParse t f (map toUpper $ format f t)

prop_parse_format_upper_named :: (Arbitrary t, Eq t, Show t, FormatTime t, ParseTime t)
                              => String -> FormatString t -> NamedProperty
prop_parse_format_upper_named = prop_named "prop_parse_format_upper" prop_parse_format_upper

-- Verify case-insensitivity with lower case.
prop_parse_format_lower :: (Eq t, FormatTime t, ParseTime t, Show t) => FormatString t -> t -> Result
prop_parse_format_lower (FormatString f) t = compareParse t f (map toLower $ format f t)

prop_parse_format_lower_named :: (Arbitrary t, Eq t, Show t, FormatTime t, ParseTime t)
                              => String -> FormatString t -> NamedProperty
prop_parse_format_lower_named = prop_named "prop_parse_format_lower" prop_parse_format_lower

prop_format_parse_format :: (FormatTime t, ParseTime t, Show t) => FormatString t -> t -> Result
prop_format_parse_format (FormatString f) t = compareResult
    (Just (format f t))
    (fmap (format f) (parse False f (format f t) `asTypeOf` Just t))

prop_format_parse_format_named :: (Arbitrary t, Show t, FormatTime t, ParseTime t)
                                  => String -> FormatString t -> NamedProperty
prop_format_parse_format_named = prop_named "prop_format_parse_format" prop_format_parse_format

--
-- * crashes in parse
--

newtype Input = Input String

instance Show Input where
    show (Input s) = s

instance Arbitrary Input where
    arbitrary = liftM Input $ list cs
      where cs = elements (['0'..'9'] ++ ['-',' ','/'] ++ ['a'..'z'] ++ ['A' .. 'Z'])
            list g = sized (\n -> choose (0,n) >>= \l -> replicateM l g)
instance CoArbitrary Input where
    coarbitrary (Input s) = coarbitrary (sum (map ord s))

prop_no_crash_bad_input :: (Eq t, ParseTime t) => FormatString t -> Input -> Property
prop_no_crash_bad_input fs@(FormatString f) (Input s) = property $
    case parse False f s of
      Nothing -> True
      Just t  -> t == t `asTypeOf` formatType fs
  where
prop_no_crash_bad_input_named :: (Eq t, ParseTime t)
                                 => String -> FormatString t -> NamedProperty
prop_no_crash_bad_input_named = prop_named "prop_no_crash_bad_input" prop_no_crash_bad_input

--
--
--

newtype FormatString a = FormatString String

formatType :: FormatString t -> t
formatType _ = undefined

castFormatString :: FormatString a -> FormatString b
castFormatString (FormatString f) = FormatString f

instance Show (FormatString a) where
    show (FormatString f) = show f

properties :: [NamedProperty]
properties =
    [("prop_fromMondayStartWeek", property prop_fromMondayStartWeek),
     ("prop_fromSundayStartWeek", property prop_fromSundayStartWeek)]
 ++ [("prop_read_show Day", property (prop_read_show :: Day -> Result)),
     ("prop_read_show TimeOfDay", property (prop_read_show :: TimeOfDay -> Result)),
     ("prop_read_show LocalTime", property (prop_read_show :: LocalTime -> Result)),
     ("prop_read_show TimeZone", property (prop_read_show :: TimeZone -> Result)),
     ("prop_read_show ZonedTime", property (prop_read_show :: ZonedTime -> Result)),
     ("prop_read_show UTCTime", property (prop_read_show :: UTCTime -> Result))]
 ++ [("prop_parse_showWeekDate", property prop_parse_showWeekDate),
     ("prop_parse_showGregorian", property prop_parse_showGregorian),
     ("prop_parse_showOrdinalDate", property prop_parse_showOrdinalDate)]

 ++ map (prop_parse_format_named "Day") dayFormats
 ++ map (prop_parse_format_named "TimeOfDay") timeOfDayFormats
 ++ map (prop_parse_format_named "LocalTime") localTimeFormats
 ++ map (prop_parse_format_named "TimeZone") timeZoneFormats
 ++ map (prop_parse_format_named "ZonedTime") zonedTimeFormats
 ++ map (prop_parse_format_named "UTCTime") utcTimeFormats

 ++ map (prop_parse_format_upper_named "Day") dayFormats
 ++ map (prop_parse_format_upper_named "TimeOfDay") timeOfDayFormats
 ++ map (prop_parse_format_upper_named "LocalTime") localTimeFormats
 ++ map (prop_parse_format_upper_named "TimeZone") timeZoneFormats
 ++ map (prop_parse_format_upper_named "ZonedTime") zonedTimeFormats
 ++ map (prop_parse_format_upper_named "UTCTime") utcTimeFormats

 ++ map (prop_parse_format_lower_named "Day") dayFormats
 ++ map (prop_parse_format_lower_named "TimeOfDay") timeOfDayFormats
 ++ map (prop_parse_format_lower_named "LocalTime") localTimeFormats
 ++ map (prop_parse_format_lower_named "TimeZone") timeZoneFormats
 ++ map (prop_parse_format_lower_named "ZonedTime") zonedTimeFormats
 ++ map (prop_parse_format_lower_named "UTCTime") utcTimeFormats

 ++ map (prop_format_parse_format_named "Day") partialDayFormats
 ++ map (prop_format_parse_format_named "TimeOfDay") partialTimeOfDayFormats
 ++ map (prop_format_parse_format_named "LocalTime") partialLocalTimeFormats
 ++ map (prop_format_parse_format_named "ZonedTime") partialZonedTimeFormats
 ++ map (prop_format_parse_format_named "UTCTime") partialUTCTimeFormats

 ++ map (prop_no_crash_bad_input_named "Day") (dayFormats ++ partialDayFormats ++ failingPartialDayFormats)
 ++ map (prop_no_crash_bad_input_named "TimeOfDay") (timeOfDayFormats ++ partialTimeOfDayFormats)
 ++ map (prop_no_crash_bad_input_named "LocalTime") (localTimeFormats ++ partialLocalTimeFormats)
 ++ map (prop_no_crash_bad_input_named "TimeZone") (timeZoneFormats)
 ++ map (prop_no_crash_bad_input_named "ZonedTime") (zonedTimeFormats ++ partialZonedTimeFormats)
 ++ map (prop_no_crash_bad_input_named "UTCTime") (utcTimeFormats ++ partialUTCTimeFormats)



dayFormats :: [FormatString Day]
dayFormats = map FormatString
    [
     -- numeric year, month, day
     "%Y-%m-%d","%Y%m%d","%C%y%m%d","%Y %m %e","%m/%d/%Y","%d/%m/%Y","%Y/%d/%m","%D %C","%F",
     -- month names
     "%Y-%B-%d","%Y-%b-%d","%Y-%h-%d",
     -- ordinal dates
     "%Y-%j",
     -- ISO week dates
     "%G-%V-%u","%G-%V-%a","%G-%V-%A","%G-%V-%w", "%A week %V, %G", "day %V, week %A, %G",
     "%G-W%V-%u",
     "%f%g-%V-%u","%f%g-%V-%a","%f%g-%V-%A","%f%g-%V-%w", "%A week %V, %f%g", "day %V, week %A, %f%g",
     "%f%g-W%V-%u",
     -- monday and sunday week dates
     "%Y-w%U-%A", "%Y-w%W-%A", "%Y-%A-w%U", "%Y-%A-w%W", "%A week %U, %Y", "%A week %W, %Y"
    ]

timeOfDayFormats :: [FormatString TimeOfDay]
timeOfDayFormats = map FormatString
    [
     -- 24 h formats
     "%H:%M:%S.%q","%k:%M:%S.%q","%H%M%S.%q","%T.%q","%X.%q","%R:%S.%q",
     "%H:%M:%S%Q","%k:%M:%S%Q","%H%M%S%Q","%T%Q","%X%Q","%R:%S%Q",
     -- 12 h formats
     "%I:%M:%S.%q %p","%I:%M:%S.%q %P","%l:%M:%S.%q %p","%r %q",
     "%I:%M:%S%Q %p","%I:%M:%S%Q %P","%l:%M:%S%Q %p","%r %Q"
    ]

localTimeFormats' :: [FormatString LocalTime]
localTimeFormats' = map FormatString $
  concat [ [df ++ " " ++ tf, tf ++ " " ++ df] | FormatString df <- dayFormats,
                                                   FormatString tf <- timeOfDayFormats]

localTimeFormats :: [FormatString LocalTime]
localTimeFormats = map FormatString [{-"%Q","%Q ","%QX"-}]

timeZoneFormats :: [FormatString TimeZone]
timeZoneFormats = map FormatString ["%z","%z%Z","%Z%z","%Z"]

zonedTimeFormats :: [FormatString ZonedTime]
zonedTimeFormats = map FormatString
  ["%a, %d %b %Y %H:%M:%S.%q %z", "%a, %d %b %Y %H:%M:%S%Q %z", "%s.%q %z", "%s%Q %z",
   "%a, %d %b %Y %H:%M:%S.%q %Z", "%a, %d %b %Y %H:%M:%S%Q %Z", "%s.%q %Z", "%s%Q %Z"]

utcTimeFormats :: [FormatString UTCTime]
utcTimeFormats = map FormatString
  ["%s.%q","%s%Q"]

--
-- * Formats that do not include all the information
--

partialDayFormats :: [FormatString Day]
partialDayFormats = map FormatString
    [ ]

partialTimeOfDayFormats :: [FormatString TimeOfDay]
partialTimeOfDayFormats = map FormatString
    [ ]

partialLocalTimeFormats :: [FormatString LocalTime]
partialLocalTimeFormats = map FormatString
    [ ]

partialZonedTimeFormats :: [FormatString ZonedTime]
partialZonedTimeFormats = map FormatString
    [
     -- %s does not include second decimals
     "%s %z",
     -- %S does not include second decimals
     "%c", "%a, %d %b %Y %H:%M:%S %Z"
    ]

partialUTCTimeFormats :: [FormatString UTCTime]
partialUTCTimeFormats = map FormatString
    [
     -- %s does not include second decimals
     "%s",
     -- %c does not include second decimals
     "%c"
    ]


--
-- * Known failures
--

knownFailures :: [NamedProperty]
knownFailures =
    map (prop_format_parse_format_named "Day") failingPartialDayFormats

failingPartialDayFormats :: [FormatString Day]
failingPartialDayFormats = map FormatString
    [ -- ISO week dates with two digit year.
      -- This can fail in the beginning or the end of a year where
      -- the ISO week date year does not match the gregorian year.
     "%g-%V-%u","%g-%V-%a","%g-%V-%A","%g-%V-%w", "%A week %V, %g", "day %V, week %A, %g",
     "%g-W%V-%u"
    ]
