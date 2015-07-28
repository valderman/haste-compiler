{-# OPTIONS -fno-warn-orphans #-}
#include "HsConfigure.h"

-- #hide
module Data.Time.Format.Parse
    (
    -- * UNIX-style parsing
#if LANGUAGE_Rank2Types
    parseTimeM, parseTimeOrError, readSTime, readPTime,
    parseTime, readTime, readsTime,
#endif
    ParseTime(..),
    -- * Locale
    module Data.Time.Format.Locale
    ) where

import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime

#if LANGUAGE_Rank2Types
import Control.Monad
#endif
import Data.Char
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Time.Format.Locale
#if LANGUAGE_Rank2Types
import Text.ParserCombinators.ReadP hiding (char, string)
#endif

#if LANGUAGE_Rank2Types
-- | Case-insensitive version of 'Text.ParserCombinators.ReadP.char'.
char :: Char -> ReadP Char
char c = satisfy (\x -> toUpper c == toUpper x)
-- | Case-insensitive version of 'Text.ParserCombinators.ReadP.string'.
string :: String -> ReadP String
string this = do s <- look; scan this s
  where
    scan []     _                               = do return this
    scan (x:xs) (y:ys) | toUpper x == toUpper y = do _ <- get; scan xs ys
    scan _      _                               = do pfail
#endif
-- | Convert string to upper case.
up :: String -> String
up = map toUpper


-- | The class of types which can be parsed given a UNIX-style time format
-- string.
class ParseTime t where
    -- | Builds a time value from a parsed input string.
    -- If the input does not include all the information needed to
    -- construct a complete value, any missing parts should be taken
    -- from 1970-01-01 00:00:00 +0000 (which was a Thursday).
    -- In the absence of @%C@ or @%Y@, century is 1969 - 2068.
    buildTime :: TimeLocale -- ^ The time locale.
              -> [(Char,String)] -- ^ Pairs of format characters and the
                                 -- corresponding part of the input.
              -> t

#if LANGUAGE_Rank2Types
-- | Parses a time value given a format string.
-- Supports the same %-codes as 'formatTime', including @%-@, @%_@ and @%0@ modifiers.
-- Case is not significant.
-- Some variations in the input are accepted:
--
-- [@%z@] accepts any of @-HHMM@ or @-HH:MM@.
--
-- [@%Z@] accepts any string of letters, or any of the formats accepted by @%z@.
--
-- [@%0Y@] accepts exactly four digits.
--
-- [@%0G@] accepts exactly four digits.
--
-- [@%0C@] accepts exactly two digits.
--
-- [@%0f@] accepts exactly two digits.
--
parseTimeM :: (Monad m,ParseTime t) =>
             Bool       -- ^ Accept leading and trailing whitespace?
          -> TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string.
          -> String     -- ^ Input string.
          -> m t    -- ^ Return the time value, or fail if the input could
                        -- not be parsed using the given format.
parseTimeM acceptWS l fmt s = case parseTimeList acceptWS l fmt s of
    [t] -> return t
    []  -> fail $ "parseTimeM: no parse of " ++ show s
    _   -> fail $ "parseTimeM: multiple parses of " ++ show s

-- | Parse a time value given a format string. Fails if the input could
-- not be parsed using the given format. See 'parseTimeM' for details.
parseTimeOrError :: ParseTime t =>
             Bool       -- ^ Accept leading and trailing whitespace?
          -> TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string.
          -> String     -- ^ Input string.
          -> t          -- ^ The time value.
parseTimeOrError acceptWS l fmt s = case parseTimeList acceptWS l fmt s of
    [t] -> t
    []  -> error $ "parseTimeOrError: no parse of " ++ show s
    _   -> error $ "parseTimeOrError: multiple parses of " ++ show s

parseTimeList :: ParseTime t =>
             Bool       -- ^ Accept leading and trailing whitespace?
          -> TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> String     -- ^ Input string.
          -> [t]
parseTimeList False l fmt s = [t | (t,"") <- readSTime False l fmt s]
parseTimeList True l fmt s = [t | (t,r) <- readSTime True l fmt s, all isSpace r]

-- | Parse a time value given a format string.  See 'parseTimeM' for details.
readSTime :: ParseTime t =>
             Bool       -- ^ Accept leading whitespace?
          -> TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> ReadS t
readSTime acceptWS l f = readP_to_S (readPTime acceptWS l f)

-- | Parse a time value given a format string.  See 'parseTimeM' for details.
readPTime :: ParseTime t =>
             Bool       -- ^ Accept leading whitespace?
          -> TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> ReadP t
readPTime False l f = readPOnlyTime l f
readPTime True l f = (skipSpaces >> readPOnlyTime l f) <++ readPOnlyTime l f

-- | Parse a time value given a format string (without allowing leading whitespace).  See 'parseTimeM' for details.
readPOnlyTime :: ParseTime t =>
             TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> ReadP t
readPOnlyTime l f = liftM (buildTime l) (parseInput l (parseFormat l f))

{-# DEPRECATED parseTime "use \"parseTimeM True\" instead" #-}
parseTime :: ParseTime t =>
             TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string.
          -> String     -- ^ Input string.
          -> Maybe t    -- ^ The time value, or 'Nothing' if the input could
                        -- not be parsed using the given format.
parseTime = parseTimeM True

{-# DEPRECATED readTime "use \"parseTimeOrError True\" instead" #-}
readTime :: ParseTime t =>
            TimeLocale -- ^ Time locale.
         -> String     -- ^ Format string.
         -> String     -- ^ Input string.
         -> t          -- ^ The time value.
readTime = parseTimeOrError True

{-# DEPRECATED readsTime "use \"readSTime True\" instead" #-}
readsTime :: ParseTime t =>
             TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> ReadS t
readsTime = readSTime True


--
-- * Internals
--

data Padding = NoPadding | SpacePadding | ZeroPadding
  deriving Show

type DateFormat = [DateFormatSpec]

data DateFormatSpec = Value (Maybe Padding) Char
                     | WhiteSpace
                     | Literal Char
  deriving Show

parseFormat :: TimeLocale -> String -> DateFormat
parseFormat l = p
  where p "" = []
        p ('%': '-' : c :cs) = (pc (Just NoPadding) c) ++ p cs
        p ('%': '_' : c :cs) = (pc (Just SpacePadding) c) ++ p cs
        p ('%': '0' : c :cs) = (pc (Just ZeroPadding) c) ++ p cs
        p ('%': c :cs) = (pc Nothing c) ++ p cs
        p (c:cs) | isSpace c = WhiteSpace : p cs
        p (c:cs) = Literal c : p cs
        pc _ 'c' = p (dateTimeFmt l)
        pc _ 'R' = p "%H:%M"
        pc _ 'T' = p "%H:%M:%S"
        pc _ 'X' = p (timeFmt l)
        pc _ 'r' = p (time12Fmt l)
        pc _ 'D' = p "%m/%d/%y"
        pc _ 'F' = p "%Y-%m-%d"
        pc _ 'x' = p (dateFmt l)
        pc _ 'h' = p "%b"
        pc _ '%' = [Literal '%']
        pc mpad c   = [Value mpad c]

parseInput :: TimeLocale -> DateFormat -> ReadP [(Char,String)]
parseInput _ [] = return []
parseInput l (Value mpad c:ff) = do
  s <- parseValue l mpad c
  r <- parseInput l ff
  return ((c,s):r)
parseInput l (Literal c:ff) = do
  _ <- char c
  parseInput l ff
parseInput l (WhiteSpace:ff) = do
  _ <- satisfy isSpace
  case ff of
     (WhiteSpace:_) -> return ()
     _ -> skipSpaces
  parseInput l ff

-- | Get the string corresponding to the given format specifier.
parseValue :: TimeLocale -> Maybe Padding -> Char -> ReadP String
parseValue l mpad c =
    case c of
      -- century
      'C' -> digits SpacePadding 2
      'f' -> digits SpacePadding 2

      -- year
      'Y' -> digits SpacePadding 4
      'G' -> digits SpacePadding 4

      -- year of century
      'y' -> digits ZeroPadding 2
      'g' -> digits ZeroPadding 2

      -- month of year
      'B' -> oneOf (map fst (months l))
      'b' -> oneOf (map snd (months l))
      'm' -> digits ZeroPadding 2

      -- day of month
      'd' -> digits ZeroPadding 2
      'e' -> digits SpacePadding 2

      -- week of year
      'V' -> digits ZeroPadding 2
      'U' -> digits ZeroPadding 2
      'W' -> digits ZeroPadding 2

      -- day of week
      'u' -> oneOf $ map (:[]) ['1'..'7']
      'a' -> oneOf (map snd (wDays l))
      'A' -> oneOf (map fst (wDays l))
      'w' -> oneOf $ map (:[]) ['0'..'6']

      -- day of year
      'j' -> digits ZeroPadding 3

      -- dayhalf of day (i.e. AM or PM)
      'P' -> oneOf (let (am,pm) = amPm l in [am, pm])
      'p' -> oneOf (let (am,pm) = amPm l in [am, pm])

      -- hour of day (i.e. 24h)
      'H' -> digits ZeroPadding 2
      'k' -> digits SpacePadding 2

      -- hour of dayhalf (i.e. 12h)
      'I' -> digits ZeroPadding 2
      'l' -> digits SpacePadding 2

      -- minute of hour
      'M' -> digits ZeroPadding 2

      -- second of minute
      'S' -> digits ZeroPadding 2

      -- picosecond of second
      'q' -> digits ZeroPadding 12
      'Q' -> liftM2 (:) (char '.') (munch isDigit) <++ return ""

      -- time zone
      'z' -> numericTZ
      'Z' -> munch1 isAlpha <++
             numericTZ <++
             return "" -- produced by %Z for LocalTime

      -- seconds since epoch
      's' -> (char '-' >> liftM ('-':) (munch1 isDigit))
             <++ munch1 isDigit

      _   -> fail $ "Unknown format character: " ++ show c
  where
    oneOf = choice . map string
    digitsforce ZeroPadding n = count n (satisfy isDigit)
    digitsforce SpacePadding _n = skipSpaces >> many1 (satisfy isDigit)
    digitsforce NoPadding _n = many1 (satisfy isDigit)
    digits pad = digitsforce (fromMaybe pad mpad)
    numericTZ = do s <- choice [char '+', char '-']
                   h <- digitsforce ZeroPadding 2
                   optional (char ':')
                   m <- digitsforce ZeroPadding 2
                   return (s:h++m)
#endif

--
-- * Instances for the time package types
--

data DayComponent = Century Integer -- century of all years
                  | CenturyYear Integer -- 0-99, last two digits of both real years and week years
                  | YearMonth Int -- 1-12
                  | MonthDay Int -- 1-31
                  | YearDay Int -- 1-366
                  | WeekDay Int -- 1-7 (mon-sun)
                  | YearWeek WeekType Int -- 1-53 or 0-53

data WeekType = ISOWeek | SundayWeek | MondayWeek

instance ParseTime Day where
    buildTime l = buildDay . concatMap (uncurry f)
     where
      f c x =
        case c of
          -- %C: century (all but the last two digits of the year), 00 - 99
          'C' -> [Century (read x)]
          -- %f century (all but the last two digits of the year), 00 - 99
          'f' -> [Century (read x)]
          -- %Y: year
          'Y' -> let y = read x in [Century (y `div` 100), CenturyYear (y `mod` 100)]
          -- %G: year for Week Date format
          'G' -> let y = read x in [Century (y `div` 100), CenturyYear (y `mod` 100)]
          -- %y: last two digits of year, 00 - 99
          'y' -> [CenturyYear (read x)]
          -- %g: last two digits of year for Week Date format, 00 - 99
          'g' -> [CenturyYear (read x)]
          -- %B: month name, long form (fst from months locale), January - December
          'B' -> [YearMonth (1 + fromJust (elemIndex (up x) (map (up . fst) (months l))))]
          -- %b: month name, short form (snd from months locale), Jan - Dec
          'b' -> [YearMonth (1 + fromJust (elemIndex (up x) (map (up . snd) (months l))))]
          -- %m: month of year, leading 0 as needed, 01 - 12
          'm' -> [YearMonth (read x)]
          -- %d: day of month, leading 0 as needed, 01 - 31
          'd' -> [MonthDay (read x)]
          -- %e: day of month, leading space as needed, 1 - 31
          'e' -> [MonthDay (read x)]
          -- %V: week for Week Date format, 01 - 53
          'V' -> [YearWeek ISOWeek (read x)]
          -- %U: week number of year, where weeks start on Sunday (as sundayStartWeek), 01 - 53
          'U' -> [YearWeek SundayWeek (read x)]
          -- %W: week number of year, where weeks start on Monday (as mondayStartWeek), 01 - 53
          'W' -> [YearWeek MondayWeek (read x)]
          -- %u: day for Week Date format, 1 - 7
          'u' -> [WeekDay (read x)]
          -- %a: day of week, short form (snd from wDays locale), Sun - Sat
          'a' -> [WeekDay (1 + (fromJust (elemIndex (up x) (map (up . snd) (wDays l))) + 6) `mod` 7)]
          -- %A: day of week, long form (fst from wDays locale), Sunday - Saturday
          'A' -> [WeekDay (1 + (fromJust (elemIndex (up x) (map (up . fst) (wDays l))) + 6) `mod` 7)]
          -- %w: day of week number, 0 (= Sunday) - 6 (= Saturday)
          'w' -> [WeekDay (((read x + 6) `mod` 7) + 1)]
          -- %j: day of year for Ordinal Date format, 001 - 366
          'j' -> [YearDay (read x)]
          _   -> []

      buildDay cs = rest cs
        where
        y = let
                d = safeLast 70 [x | CenturyYear x <- cs]
                c = safeLast (if d >= 69 then 19 else 20) [x | Century x <- cs]
             in 100 * c + d

        rest (YearMonth m:_)  = let d = safeLast 1 [x | MonthDay x <- cs]
                             in fromGregorian y m d
        rest (YearDay d:_) = fromOrdinalDate y d
        rest (YearWeek wt w:_) = let d = safeLast 4 [x | WeekDay x <- cs]
                              in case wt of
                                   ISOWeek    -> fromWeekDate y w d
                                   SundayWeek -> fromSundayStartWeek y w (d `mod` 7)
                                   MondayWeek -> fromMondayStartWeek y w d
        rest (_:xs)        = rest xs
        rest []            = rest [YearMonth 1]

      safeLast x xs = last (x:xs)

instance ParseTime TimeOfDay where
    buildTime l = foldl f midnight
        where
          f t@(TimeOfDay h m s) (c,x) =
              case c of
                'P' -> if up x == fst (amPm l) then am else pm
                'p' -> if up x == fst (amPm l) then am else pm
                'H' -> TimeOfDay (read x) m s
                'I' -> TimeOfDay (read x) m s
                'k' -> TimeOfDay (read x) m s
                'l' -> TimeOfDay (read x) m s
                'M' -> TimeOfDay h (read x) s
                'S' -> TimeOfDay h m (fromInteger (read x))
                'q' -> TimeOfDay h m (mkPico (truncate s) (read x))
                'Q' -> if null x then t
                        else let ps = read $ take 12 $ rpad 12 '0' $ drop 1 x
                              in TimeOfDay h m (mkPico (truncate s) ps)
                _   -> t
            where am = TimeOfDay (h `mod` 12) m s
                  pm = TimeOfDay (if h < 12 then h + 12 else h) m s

rpad :: Int -> a -> [a] -> [a]
rpad n c xs = xs ++ replicate (n - length xs) c

mkPico :: Integer -> Integer -> Pico
mkPico i f = fromInteger i + fromRational (f % 1000000000000)

instance ParseTime LocalTime where
    buildTime l xs = LocalTime (buildTime l xs) (buildTime l xs)

enumDiff :: (Enum a) => a -> a -> Int
enumDiff a b = (fromEnum a) - (fromEnum b)

getMilZoneHours :: Char -> Maybe Int
getMilZoneHours c | c < 'A' = Nothing
getMilZoneHours c | c <= 'I' = Just $ 1 + enumDiff c 'A'
getMilZoneHours 'J' = Nothing
getMilZoneHours c | c <= 'M' = Just $ 10 + enumDiff c 'K'
getMilZoneHours c | c <= 'Y' = Just $ (enumDiff 'N' c) - 1
getMilZoneHours 'Z' = Just 0
getMilZoneHours _ = Nothing

instance ParseTime TimeZone where
    buildTime l = foldl f (minutesToTimeZone 0)
      where
        f t@(TimeZone offset dst name) (c,x) =
            case c of
              'z' -> zone
              'Z' | null x           -> t
                  | isAlpha (head x) -> let y = up x in
                      case find (\tz -> y == timeZoneName tz) (knownTimeZones l) of
                        Just tz -> tz
                        Nothing -> case y of
                            [yc] | Just hours <- getMilZoneHours yc -> TimeZone (hours * 60) False y
                            _ -> TimeZone offset dst y
                  | otherwise        -> zone
              _   -> t
          where zone = TimeZone (readTzOffset x) dst name

readTzOffset :: String -> Int
readTzOffset str =
    case str of
      (s:h1:h2:':':m1:m2:[]) -> calc s h1 h2 m1 m2
      (s:h1:h2:m1:m2:[]) -> calc s h1 h2 m1 m2
      _ -> 0
    where calc s h1 h2 m1 m2 = sign * (60 * h + m)
              where sign = if s == '-' then -1 else 1
                    h = read [h1,h2]
                    m = read [m1,m2]

instance ParseTime ZonedTime where
    buildTime l xs = foldl f (ZonedTime (buildTime l xs) (buildTime l xs)) xs
        where
          f t@(ZonedTime (LocalTime _ tod) z) (c,x) =
              case c of
                's' -> let s = fromInteger (read x)
                           (_,ps) = properFraction (todSec tod) :: (Integer,Pico)
                           s' = s + fromRational (toRational ps)
                        in utcToZonedTime z (posixSecondsToUTCTime s')
                _   -> t

instance ParseTime UTCTime where
    buildTime l = zonedTimeToUTC . buildTime l

-- * Read instances for time package types

#if LANGUAGE_Rank2Types
instance Read Day where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%Y-%m-%d"

instance Read TimeOfDay where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%H:%M:%S%Q"

instance Read LocalTime where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

instance Read TimeZone where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%Z"

instance Read ZonedTime where
    readsPrec n = readParen False $ \s ->
        [(ZonedTime t z, r2) | (t,r1) <- readsPrec n s, (z,r2) <- readsPrec n r1]

instance Read UTCTime where
    readsPrec n s = [ (zonedTimeToUTC t, r) | (t,r) <- readsPrec n s ]
#endif

