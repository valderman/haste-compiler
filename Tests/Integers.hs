module Tests.Integers where
import Data.Bits

{-# NOINLINE num1 #-}
num1 :: Integer
num1 = 0xffffffffffff

{-# NOINLINE num2 #-}
num2 :: Integer
num2 = 4234509348702334235435346

runTest :: IO [Integer]
runTest = return [
  num1,
  num1 + num2,
  num1 * num2,
  num2 `div` num1,
  (num2 `xor` num1) .&. 43042900409]
