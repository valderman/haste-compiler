{-# LANGUAGE CPP #-}
module Tests.HasteReadInt where
#ifdef HASTE
import Haste

{-# NOINLINE x #-}
x :: Int
x = read_ "27"

runTest :: IO Int
runTest = return x
#else

{-# NOINLINE x #-}
x :: Int
x = read "27"

runTest :: IO Int
runTest = return x
#endif
