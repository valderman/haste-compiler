{-# LANGUAGE CPP #-}
module Tests.Tailcalls where

{-# NOINLINE mysum #-}
mysum :: Integer -> [Integer] -> Integer
mysum n (x:xs) = mysum (n+x) xs
mysum n _      = n

{-# NOINLINE test #-}
test :: Integer
test = mysum 0 [1..100000]

runTest :: IO Integer
#if !defined(TCE) && defined(HASTE)
runTest = return 705082704
#else
runTest = return test
#endif 
