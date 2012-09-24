{-# LANGUAGE CPP #-}
module Tests.DoubleConversion where

#ifdef __HASTE__
import Haste
strFloor = show_ . (floor_ :: Double -> Int)
strCeiling = show_ . (ceiling_ :: Double -> Int)
strRound = show_ . (round_ :: Double -> Int)
#else
strFloor = show . (floor :: Double -> Int)
strCeiling = show . (ceiling :: Double -> Int)
strRound = show . (round :: Double -> Int)
#endif

{-# NOINLINE cases #-}
cases :: [Double]
cases = [-1, -1.2, -1.7, 0, 1, 1.2, 1.7]

runTest :: IO [String]
runTest = return $ map strFloor cases ++ map strCeiling cases ++ map strRound cases
