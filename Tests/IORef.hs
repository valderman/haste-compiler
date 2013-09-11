module Tests.IORef where

import Data.IORef

runTest :: IO ()
runTest = do
    ref <- newIORef 23
    atomicModifyIORef ref $ \ _ -> (42, ())
    print =<< readIORef ref
