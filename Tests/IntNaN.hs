module Tests.IntNaN where

{-# NOINLINE x #-}
x :: IO Double
x = return 0

runTest :: IO ()
runTest = do
  x' <- x
  print (x'/x')
  print (round (x'/x') :: Int)
