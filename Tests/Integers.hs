module Tests.Integers where
import Data.Bits

-- TODO: at the moment, it's only possible to specify signed 32 bit integer
--       literals!

{-# NOINLINE num1 #-}
num1 :: Integer
num1 = 0xfffffff

{-# NOINLINE num2 #-}
num2 :: Integer
num2 = 423450934

{-# NOINLINE num3 #-}
num3 :: Integer
num3 = 50000

{-# NOINLINE num4 #-}
num4 :: Integer
num4 = num3*num3+num2

runTest :: IO [Integer]
runTest = return [
  num1,
  num1 + num2,
  num1 * num2,
  num2 `div` num1,
  num2 `mod` num1,
  num2 `quot` num1,
  num2 `rem` num1,
  num2 - num1,
  (num2 `xor` num1) .&. 43042900,
  num4]
