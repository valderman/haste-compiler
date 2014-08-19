{-# LANGUAGE CPP #-}
module Tests.IOThunk where
#ifdef __HASTE__
import Haste
#endif

runTest :: IO ()
runTest = do
  x ; x ; x
  y ; y ; y

{-# NOINLINE x #-}
x :: IO ()
x = putStrLn "hello!"

y :: IO ()
y = putStrLn "hi!"
