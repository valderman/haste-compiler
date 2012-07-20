{-# LANGUAGE CPP #-}
module Tests.WordOverflow where
import Data.Word

#ifdef HASTE
type Wordish = Word
#else
type Wordish = Word32
#endif

{-# NOINLINE int_max #-}
int_max :: Wordish
int_max = 0xffffffff

{-# NOINLINE int_min #-}
int_min :: Wordish
int_min = 0

{-# NOINLINE num #-}
num :: IO Wordish
num = return 0xffffffff

runTest :: IO [Wordish]
runTest = do
  num' <- num
  return [
    int_max + 27,
    int_min - 1,
    num' * num']
