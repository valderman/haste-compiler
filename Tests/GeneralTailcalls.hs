module Tests.GeneralTailcalls where

newtype Cont a = Cont (Cont a -> a -> a)

{-# NOINLINE f #-}
f :: Cont Int -> Int -> Int
f _ 0           = 0
f (Cont cont) n = cont (Cont f) (n-1)

{-# NOINLINE g #-}
g :: Cont Int -> Int -> Int
g _ 0           = 0
g (Cont cont) n = cont (Cont g) (n-1)

runTest :: IO Int
runTest = return $ f (Cont g) 1000000
