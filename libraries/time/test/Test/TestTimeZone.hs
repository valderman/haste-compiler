{-# OPTIONS -Wall -Werror #-}

module Test.TestTimeZone where

import Data.Time
import System.Posix.Env (putEnv)
import Test.TestUtil

testTimeZone :: Test
testTimeZone = ioTest "getTimeZone respects TZ env var" $ do
  putEnv "TZ=UTC+0"
  zone1 <- getTimeZone epoch
  putEnv "TZ=EST+5"
  zone2 <- getTimeZone epoch
  return $ diff False (zone1 == zone2)
 where
  epoch = UTCTime (ModifiedJulianDay 0) 0
