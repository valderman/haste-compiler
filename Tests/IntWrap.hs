module Tests.IntWrap where

{-# NOINLINE int_max #-}
int_max :: Int
int_max = 0x7fffffff

{-# NOINLINE int_min #-}
int_min :: Int
int_min = -2147483648

{-# NOINLINE num #-}
num :: Int
num = 0xfffff

runTest :: IO [Int]
runTest = return [
  int_max + 27,
  int_min - 1,
  num * num]
