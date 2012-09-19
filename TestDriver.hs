{-# LANGUAGE CPP #-}
module Main where
#ifdef __HASTE__
import Haste
#endif
import Tests.TEST_MODULE (runTest)

#ifdef __HASTE__
main = do
  res <- runTest
  alert $ show res
#else
main = do
  res <- runTest
  putStrLn $ show res
#endif
