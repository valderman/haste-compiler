module Tests.IntegerGcdLcm where
import GHC.Integer.GMP.Internals

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
num4 = 93

{-# NOINLINE num5 #-}
num5 :: Integer
num5 = 50

runTest :: IO [Integer]
runTest = return [gcdInteger num1 num2,
                  gcdInteger num2 num3,
                  gcdInteger num3 num4,
                  gcdInteger num3 num5,
                  lcmInteger num1 num2,
                  lcmInteger num2 num3,
                  lcmInteger num3 num4,
                  lcmInteger num3 num5]
