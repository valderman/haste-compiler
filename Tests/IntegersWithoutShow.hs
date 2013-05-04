module Tests.IntegersWithoutShow where
import Data.Bits

myshow :: Integer -> String
myshow n =
  case n `quotRem` 10 of
    (0, n)    -> showDigit n
    (rest, n) -> myshow rest ++ showDigit n
  where
    showDigit 0 = "0"
    showDigit 1 = "1"
    showDigit 2 = "2"
    showDigit 3 = "3"
    showDigit 4 = "4"
    showDigit 5 = "5"
    showDigit 6 = "6"
    showDigit 7 = "7"
    showDigit 8 = "8"
    showDigit 9 = "9"

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

runTest :: IO [String]
runTest = return $ map myshow [
  num1,
  num1 + num2,
  num1 * num2,
  num2 `div` num1,
  (num2 `xor` num1) .&. 43042900,
  num4]
