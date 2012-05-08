{-# LANGUAGE CPP #-}
module Tests.IntWrap where
import Data.Int

#ifdef HASTE
type Intish = Int
#else
type Intish = Int32
#endif

{-# NOINLINE int_max #-}
int_max :: Intish
int_max = 0x7fffffff

{-# NOINLINE int_min #-}
int_min :: Intish
int_min = -2147483648

{-# NOINLINE num #-}
num :: Intish
num = 0xfffff

runTest :: IO [Intish]
runTest = return [
  int_max + 27,
  int_min - 1,
  num * num]
