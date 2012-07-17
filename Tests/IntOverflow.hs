{-# LANGUAGE CPP #-}
module Tests.IntOverflow where
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
num :: IO Intish
num = return 1999999999

runTest :: IO [Intish]
runTest = do
  num' <- num
  return [
    int_max + 27,
    int_min - 1,
    num' * num']
