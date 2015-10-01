module Tests.IntNaN where

{-# NOINLINE x #-}
x :: IO Double
x = return 0

runTest :: IO ()
runTest = do
  x' <- x
  print $ isNaN (x'/x')
  print $ 0 == (round (x'/x') :: Int)
