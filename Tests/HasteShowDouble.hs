{-# LANGUAGE CPP #-}
module Tests.HasteShowDouble where

{-# INLINE twentyseven #-}
twentyseven :: Double
twentyseven = 27

runTest :: IO String
runTest = return $ show twentyseven
