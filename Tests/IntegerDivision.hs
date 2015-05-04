module Tests.IntegerDivision where

{-# NOINLINE x #-}
x :: IO Integer
x = return 1000000000000

runTest = do
  x' <- x
  return $ x' `div` 10
