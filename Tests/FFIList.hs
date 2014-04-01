{-# LANGUAGE CPP, OverloadedStrings #-}
module Tests.FFIList where
import Haste.Foreign

sumup :: [Int] -> IO [Int]
#ifdef __HASTE__
sumup = ffi "(function(xs){var x=0; for(var i in xs) {x+=xs[i];} return [x];})"
#else
sumup = return . sum
#endif

runTest :: IO [Int]
runTest = sumup [1..10]
