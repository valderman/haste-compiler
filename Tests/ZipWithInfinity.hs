{-# LANGUAGE CPP #-}
module Tests.ZipWithInfinity where
import Haste

nums :: [Int]
nums = foldr (zipWith (+)) (repeat 0) [[0,1],[1,0]]

runTest :: IO ()
runTest = do
#ifdef __HASTE__
  writeLog $ show nums
#else
  putStrLn $ show nums
#endif
