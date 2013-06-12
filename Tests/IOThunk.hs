{-# LANGUAGE CPP #-}
module Tests.IOThunk where
#ifdef __HASTE__
import Haste
output = alert
#else
output = putStrLn
#endif

runTest :: IO ()
runTest = do
  x ; x ; x
  y ; y ; y

{-# NOINLINE x #-}
x :: IO ()
x = output "hello!"

y :: IO ()
y = output "hi!"
