{-# LANGUAGE CPP #-}
module Tests.HasteShowDouble where
#ifdef HASTE
import Haste

runTest :: IO String
runTest = return $ show_ (27 :: Double)
#else
runTest :: IO String
runTest = return $ show (27 :: Double)
#endif
