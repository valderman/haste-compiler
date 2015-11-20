{-# LANGUAGE StaticPointers #-}
module Tests.StaticPtr where
import GHC.StaticPtr

runTest :: IO ()
runTest = do
  print (staticKey (static staticPtrKeys))
  print (staticKey (static runTest))
