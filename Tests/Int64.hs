module Tests.Int64 where
import Data.Int
import Data.Bits

{-# NOINLINE num1 #-}
num1 :: Int64
num1 = 0xfffffffff

{-# NOINLINE num2 #-}
num2 :: Int64
num2 = 423450934

{-# NOINLINE num3 #-}
num3 :: Int64
num3 = 50000

{-# NOINLINE num4 #-}
num4 :: Int64
num4 = num3*num3+num2

runTest :: IO [Int64]
runTest = return [
  num1,
  num1 + num2,
  num1 * num2,
  num2 `div` num1,
  (num2 `xor` num1) .&. 43042900,
  num4]
