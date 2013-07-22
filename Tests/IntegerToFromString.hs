{-# LANGUAGE CPP #-}
-- | Tests the fast toString/fromString Haste specific functions.
module Tests.IntegerToFromString where

#ifdef __HASTE__
import Haste
fromS = fromString
toS = toString
#else
fromS s =
  case reads s of
    [(n, "")] -> Just n
    _         -> Nothing
toS = show
#endif

runTest :: IO String
runTest =
  case fromS "3450834053468865983456093" of
    Just x -> return $ toS $ x + (1 :: Integer)
