module Tests.Weak where

import Control.Applicative ((<$>))
import Control.Concurrent
import System.Mem.Weak

sampleFinalizer::MVar String->IO ()
sampleFinalizer var = putMVar var "Done!"

runTest :: IO String
runTest = do
  let val = "test string"

  var <- newEmptyMVar
  weakptr <- mkWeak 1 val . Just . sampleFinalizer $ var
  val' <- deRefWeak weakptr
  finalize weakptr
  finalizerTest <- takeMVar var

  return $ show val' ++ " " ++ finalizerTest
