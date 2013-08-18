module Tests.MVar where

import Control.Concurrent

runTest :: IO String
runTest = do
  v <- newMVar "hi!"
  x <- takeMVar v
  putMVar v "bye!"
  y <- takeMVar v
  return $ x ++ y
