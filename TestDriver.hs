{-# LANGUAGE CPP #-}
module Main where
#ifdef __HASTE__
import Haste
#endif
import Tests.TEST_MODULE (runTest)

main = do
  res <- runTest
  putStrLn $ show res
