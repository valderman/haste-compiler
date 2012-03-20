{-# LANGUAGE CPP #-}
module Main where
#ifdef HASTE
import Haste
#endif
import Tests.TEST_MODULE (runTest)

#ifdef HASTE
main = do
  res <- runTest
  alert $ show res
#else
main = do
  res <- runTest
  putStrLn $ show res
#endif
