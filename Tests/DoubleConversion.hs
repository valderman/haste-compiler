{-# LANGUAGE CPP #-}
module Tests.DoubleConversion where

{-# NOINLINE cases #-}
cases :: [Double]
cases = [-1, -1.2, -1.7, 0, 1, 1.2, 1.7]

runTest :: IO [Int]
runTest = return $ map floor cases ++ map ceiling cases ++ map round cases
