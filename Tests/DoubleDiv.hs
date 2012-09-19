{-# LANGUAGE CPP #-}
module Tests.DoubleDiv where

#ifdef __HASTE__
import Haste
str = show_ . (round_ :: Double -> Int)
#else
str = show . (round :: Double -> Int)
#endif

{-# NOINLINE a #-}
a :: Double
a = 1000000000

{-# NOINLINE b #-}
b :: Double
b = 34029340939

{-# NOINLINE c #-}
c :: Double
c = 234

runTest :: IO [String]
runTest = return [
  str (a/a), str (a/b), str (a/c),
  str (b/a), str (b/b), str (b/c),
  str (c/a), str (c/b), str (c/c)]
