{-# LANGUAGE CPP #-}
module Tests.HasteReadInt where
#ifdef __HASTE__
import Haste

{-# NOINLINE x #-}
x :: Int
Just x = fromString "27"

runTest :: IO Int
runTest = return x
#else

{-# NOINLINE x #-}
x :: Int
x = read "27"

runTest :: IO Int
runTest = return x
#endif
