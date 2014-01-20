module Tests.Word64 where
import Data.Word
import Data.Bits

{-# NOINLINE int_max #-}
int_max :: Word64
int_max = 0xffffffffffffffff

{-# NOINLINE int_min #-}
int_min :: Word64
int_min = 0

{-# NOINLINE num #-}
num :: IO Word64
num = return 0xffffffffffffffff

{-# NOINLINE num1 #-}
num1 :: Word64
num1 = 0xfffffff

{-# NOINLINE num2 #-}
num2 :: Word64
num2 = 9898435943303

{-# NOINLINE num3 #-}
num3 :: Word64
num3 = -34209

{-# NOINLINE num4 #-}
num4 :: Word64
num4 = num3*num3+num2

runTest :: IO [Word64]
runTest = do
  num' <- num
  return [
    int_max + 27,
    int_min - 1,
    num' * num',
    num1,
    num1 + num2,
    num1 * num2,
    num2 `div` num1,
    (num2 `xor` num1) .&. 43042900,
    num4]
