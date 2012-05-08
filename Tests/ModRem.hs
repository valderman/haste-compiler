module Tests.ModRem where

{-# NOINLINE a #-}
a :: Int
a = 15

{-# NOINLINE b #-}
b :: Int
b = 27

runTest :: IO [Int]
runTest = return [
  a `mod` b,
  b `mod` a,
  a `rem` b,
  b `rem` a]
