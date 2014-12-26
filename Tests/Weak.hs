module Tests.Weak where

import System.Mem.Weak

runTest :: IO String
runTest = do
  let v = "test string"
  p1 <- mkWeak 1 v Nothing

  p1' <- deRefWeak p1

  return $ show p1'
