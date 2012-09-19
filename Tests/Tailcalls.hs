{-# LANGUAGE CPP #-}
module Tests.Tailcalls where

{-# NOINLINE mysum #-}
mysum :: Integer -> [Integer] -> Integer
mysum n (x:xs) = mysum (n+x) xs
mysum n _      = n

{-# NOINLINE num #-}
num :: Integer
num = 70711

{-# NOINLINE test #-}
test :: Integer
#if defined(O2) || defined(__HASTE_TCE__)
test = mysum 0 [1..100000]
#else
test = num^2+4479
#endif

runTest :: IO Integer
runTest = return test
