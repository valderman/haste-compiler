module Data.Time.Format
	(
	-- * UNIX-style formatting
	NumericPadOption,FormatTime(..),formatTime,
	module Data.Time.Format.Parse
	) where

import Data.Time.Format.Parse
import Data.Time.LocalTime
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar
import Data.Time.Calendar.Private
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Data.Maybe
import Data.Char
import Data.Fixed

-- <http://www.opengroup.org/onlinepubs/007908799/xsh/strftime.html>
class FormatTime t where
	formatCharacter :: Char -> Maybe (TimeLocale -> Maybe NumericPadOption -> t -> String)

formatChar :: (FormatTime t) => Char -> TimeLocale -> Maybe NumericPadOption -> t -> String
formatChar '%' _ _ _ = "%"
formatChar 't' _ _ _ = "\t"
formatChar 'n' _ _ _ = "\n"
formatChar c locale mpado t = case (formatCharacter c) of
	Just f -> f locale mpado t
	_ -> ""

-- | Substitute various time-related information for each %-code in the string, as per 'formatCharacter'.
--
-- For all types (note these three are done here, not by 'formatCharacter'):
--
-- [@%%@] @%@
--
-- [@%t@] tab
--
-- [@%n@] newline
--
-- glibc-style modifiers can be used before the letter (here marked as @z@):
--
-- [@%-z@] no padding
--
-- [@%_z@] pad with spaces
--
-- [@%0z@] pad with zeros
--
-- [@%^z@] convert to upper case
--
-- [@%#z@] convert to lower case (consistently, unlike glibc)
--
-- For 'TimeZone' (and 'ZonedTime' and 'UTCTime'):
--
-- [@%z@] timezone offset in the format @-HHMM@.
--
-- [@%Z@] timezone name
--
-- For 'LocalTime' (and 'ZonedTime' and 'UTCTime'):
--
-- [@%c@] as 'dateTimeFmt' @locale@ (e.g. @%a %b %e %H:%M:%S %Z %Y@)
--
-- For 'TimeOfDay' (and 'LocalTime' and 'ZonedTime' and 'UTCTime'):
--
-- [@%R@] same as @%H:%M@
--
-- [@%T@] same as @%H:%M:%S@
--
-- [@%X@] as 'timeFmt' @locale@ (e.g. @%H:%M:%S@)
--
-- [@%r@] as 'time12Fmt' @locale@ (e.g. @%I:%M:%S %p@)
--
-- [@%P@] day-half of day from ('amPm' @locale@), converted to lowercase, @am@, @pm@
--
-- [@%p@] day-half of day from ('amPm' @locale@), @AM@, @PM@
--
-- [@%H@] hour of day (24-hour), 0-padded to two chars, @00@ - @23@
--
-- [@%k@] hour of day (24-hour), space-padded to two chars, @ 0@ - @23@
--
-- [@%I@] hour of day-half (12-hour), 0-padded to two chars, @01@ - @12@
--
-- [@%l@] hour of day-half (12-hour), space-padded to two chars, @ 1@ - @12@
--
-- [@%M@] minute of hour, 0-padded to two chars, @00@ - @59@
--
-- [@%S@] second of minute (without decimal part), 0-padded to two chars, @00@ - @60@
--
-- [@%q@] picosecond of second, 0-padded to twelve chars, @000000000000@ - @999999999999@.
--
-- [@%Q@] decimal point and fraction of second, up to 12 second decimals, without trailing zeros.
-- For a whole number of seconds, @%Q@ produces the empty string.
--
-- For 'UTCTime' and 'ZonedTime':
--
-- [@%s@] number of whole seconds since the Unix epoch. For times before
-- the Unix epoch, this is a negative number. Note that in @%s.%q@ and @%s%Q@ 
-- the decimals are positive, not negative. For example, 0.9 seconds
-- before the Unix epoch is formatted as @-1.1@ with @%s%Q@.
--
-- For 'Day' (and 'LocalTime' and 'ZonedTime' and 'UTCTime'):
--
-- [@%D@] same as @%m\/%d\/%y@
--
-- [@%F@] same as @%Y-%m-%d@
--
-- [@%x@] as 'dateFmt' @locale@ (e.g. @%m\/%d\/%y@)
--
-- [@%Y@] year, no padding. Note @%0y@ and @%_y@ pad to four chars
--
-- [@%y@] year of century, 0-padded to two chars, @00@ - @99@
--
-- [@%C@] century, no padding. Note @%0C@ and @%_C@ pad to two chars
--
-- [@%B@] month name, long form ('fst' from 'months' @locale@), @January@ - @December@
--
-- [@%b@, @%h@] month name, short form ('snd' from 'months' @locale@), @Jan@ - @Dec@
--
-- [@%m@] month of year, 0-padded to two chars, @01@ - @12@
--
-- [@%d@] day of month, 0-padded to two chars, @01@ - @31@
--
-- [@%e@] day of month, space-padded to two chars,  @ 1@ - @31@
--
-- [@%j@] day of year, 0-padded to three chars, @001@ - @366@
--
-- [@%G@] year for Week Date format, no padding. Note @%0G@ and @%_G@ pad to four chars
--
-- [@%g@] year of century for Week Date format, 0-padded to two chars, @00@ - @99@
--
-- [@%f@] century for Week Date format, no padding. Note @%0f@ and @%_f@ pad to two chars
--
-- [@%V@] week of year for Week Date format, 0-padded to two chars, @01@ - @53@
--
-- [@%u@] day of week for Week Date format, @1@ - @7@
--
-- [@%a@] day of week, short form ('snd' from 'wDays' @locale@), @Sun@ - @Sat@
--
-- [@%A@] day of week, long form ('fst' from 'wDays' @locale@), @Sunday@ - @Saturday@
--
-- [@%U@] week of year where weeks start on Sunday (as 'sundayStartWeek'), 0-padded to two chars, @00@ - @53@
--
-- [@%w@] day of week number, @0@ (= Sunday) - @6@ (= Saturday)
--
-- [@%W@] week of year where weeks start on Monday (as 'mondayStartWeek'), 0-padded to two chars, @00@ - @53@
formatTime :: (FormatTime t) => TimeLocale -> String -> t -> String
formatTime _ [] _ = ""
formatTime locale ('%':'_':c:cs) t = (formatChar c locale (Just (Just ' ')) t) ++ (formatTime locale cs t)
formatTime locale ('%':'-':c:cs) t = (formatChar c locale (Just Nothing) t) ++ (formatTime locale cs t)
formatTime locale ('%':'0':c:cs) t = (formatChar c locale (Just (Just '0')) t) ++ (formatTime locale cs t)
formatTime locale ('%':'^':c:cs) t = (fmap toUpper (formatChar c locale Nothing t)) ++ (formatTime locale cs t)
formatTime locale ('%':'#':c:cs) t = (fmap toLower (formatChar c locale Nothing t)) ++ (formatTime locale cs t)
formatTime locale ('%':c:cs) t = (formatChar c locale Nothing t) ++ (formatTime locale cs t)
formatTime locale (c:cs) t = c:(formatTime locale cs t)

instance FormatTime LocalTime where
	formatCharacter 'c' = Just (\locale _ -> formatTime locale (dateTimeFmt locale))
	formatCharacter c = case (formatCharacter c) of
		Just f -> Just (\locale mpado dt -> f locale mpado (localDay dt))
		Nothing -> case (formatCharacter c) of
			Just f -> Just (\locale mpado dt -> f locale mpado (localTimeOfDay dt))
			Nothing -> Nothing

instance FormatTime TimeOfDay where
	-- Aggregate
	formatCharacter 'R' = Just (\locale _ -> formatTime locale "%H:%M")
	formatCharacter 'T' = Just (\locale _ -> formatTime locale "%H:%M:%S")
	formatCharacter 'X' = Just (\locale _ -> formatTime locale (timeFmt locale))
	formatCharacter 'r' = Just (\locale _ -> formatTime locale (time12Fmt locale))
	-- AM/PM
	formatCharacter 'P' = Just (\locale _ day -> map toLower ((if (todHour day) < 12 then fst else snd) (amPm locale)))
	formatCharacter 'p' = Just (\locale _ day -> (if (todHour day) < 12 then fst else snd) (amPm locale))
	-- Hour
	formatCharacter 'H' = Just (\_ opt -> (show2 (fromMaybe (Just '0') opt)) . todHour)
	formatCharacter 'I' = Just (\_ opt -> (show2 (fromMaybe (Just '0') opt)) . (\h -> (mod (h - 1) 12) + 1) . todHour)
	formatCharacter 'k' = Just (\_ opt -> (show2 (fromMaybe (Just ' ') opt)) . todHour)
	formatCharacter 'l' = Just (\_ opt -> (show2 (fromMaybe (Just ' ') opt)) . (\h -> (mod (h - 1) 12) + 1) . todHour)
	-- Minute
	formatCharacter 'M' = Just (\_ opt -> (show2 (fromMaybe (Just '0') opt)) . todMin)
	-- Second
	formatCharacter 'S' = Just (\_ opt -> (show2 (fromMaybe (Just '0') opt) :: Int -> String) . truncate . todSec)
	formatCharacter 'q' = Just (\_ _ -> drop 1 . dropWhile (/='.') . showFixed False . todSec)
	formatCharacter 'Q' = Just (\_ _ -> dropWhile (/='.') . showFixed True . todSec)

	-- Default
	formatCharacter _   = Nothing

instance FormatTime ZonedTime where
	formatCharacter 'c' = Just (\locale _ -> formatTime locale (dateTimeFmt locale))
	formatCharacter 's' = Just (\_ _ zt -> show (floor (utcTimeToPOSIXSeconds (zonedTimeToUTC zt)) :: Integer))
	formatCharacter c = case (formatCharacter c) of
		Just f -> Just (\locale mpado dt -> f locale mpado (zonedTimeToLocalTime dt))
		Nothing -> case (formatCharacter c) of
			Just f -> Just (\locale mpado dt -> f locale mpado (zonedTimeZone dt))
			Nothing -> Nothing

instance FormatTime TimeZone where
	formatCharacter 'z' = Just (\_ opt -> timeZoneOffsetString' (fromMaybe (Just '0') opt))
	formatCharacter 'Z' = 
            Just (\_ opt z -> let n = timeZoneName z
                           in if null n then timeZoneOffsetString' (fromMaybe (Just '0') opt) z else n)
	formatCharacter _ = Nothing

instance FormatTime Day where
	-- Aggregate
	formatCharacter 'D' = Just (\locale _ -> formatTime locale "%m/%d/%y")
	formatCharacter 'F' = Just (\locale _ -> formatTime locale "%Y-%m-%d")
	formatCharacter 'x' = Just (\locale _ -> formatTime locale (dateFmt locale))

	-- Year Count
	formatCharacter 'Y' = Just (\_ opt -> (show4 (fromMaybe Nothing opt)) . fst . toOrdinalDate)
	formatCharacter 'y' = Just (\_ opt -> (show2 (fromMaybe (Just '0') opt)) . mod100 . fst . toOrdinalDate)
	formatCharacter 'C' = Just (\_ opt -> (show2 (fromMaybe Nothing opt)) . div100 . fst . toOrdinalDate)
	-- Month of Year
	formatCharacter 'B' = Just (\locale _ -> fst . (\(_,m,_) -> (months locale) !! (m - 1)) . toGregorian)
	formatCharacter 'b' = Just (\locale _ -> snd . (\(_,m,_) -> (months locale) !! (m - 1)) . toGregorian)
	formatCharacter 'h' = Just (\locale _ -> snd . (\(_,m,_) -> (months locale) !! (m - 1)) . toGregorian)
	formatCharacter 'm' = Just (\_ opt -> (show2 (fromMaybe (Just '0') opt)) . (\(_,m,_) -> m) . toGregorian)
	-- Day of Month
	formatCharacter 'd' = Just (\_ opt -> (show2 (fromMaybe (Just '0') opt)) . (\(_,_,d) -> d) . toGregorian)
	formatCharacter 'e' = Just (\_ opt -> (show2 (fromMaybe (Just ' ') opt)) . (\(_,_,d) -> d) . toGregorian)
	-- Day of Year
	formatCharacter 'j' = Just (\_ opt -> (show3 (fromMaybe (Just '0') opt)) . snd . toOrdinalDate)

	-- ISO 8601 Week Date
	formatCharacter 'G' = Just (\_ opt -> (show4 (fromMaybe Nothing opt)) . (\(y,_,_) -> y) . toWeekDate)
	formatCharacter 'g' = Just (\_ opt -> (show2 (fromMaybe (Just '0') opt)) . mod100 . (\(y,_,_) -> y) . toWeekDate)
	formatCharacter 'f' = Just (\_ opt -> (show2 (fromMaybe Nothing opt)) . div100 . (\(y,_,_) -> y) . toWeekDate)

	formatCharacter 'V' = Just (\_ opt -> (show2 (fromMaybe (Just '0') opt)) . (\(_,w,_) -> w) . toWeekDate)
	formatCharacter 'u' = Just (\_ _ -> show . (\(_,_,d) -> d) . toWeekDate)

	-- Day of week
	formatCharacter 'a' = Just (\locale _ -> snd . ((wDays locale) !!) . snd . sundayStartWeek)
	formatCharacter 'A' = Just (\locale _ -> fst . ((wDays locale) !!) . snd . sundayStartWeek)
	formatCharacter 'U' = Just (\_ opt -> (show2 (fromMaybe (Just '0') opt)) . fst . sundayStartWeek)
	formatCharacter 'w' = Just (\_ _ -> show . snd . sundayStartWeek)
	formatCharacter 'W' = Just (\_ opt -> (show2 (fromMaybe (Just '0') opt)) . fst . mondayStartWeek)
	
	-- Default
	formatCharacter _   = Nothing

instance FormatTime UTCTime where
	formatCharacter c = fmap (\f locale mpado t -> f locale mpado (utcToZonedTime utc t)) (formatCharacter c)
