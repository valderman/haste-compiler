{-# LANGUAGE CPP, OverloadedStrings #-}
module Tests.FFITuples where
import Haste.Foreign

sumup :: (Int, Double, Float) -> IO (String, String)
#ifdef __HASTE__
sumup = ffi "(function(xs){return ['ok', ''+(xs[0]+xs[1]+xs[2])];})"
#else
sumup (a,b,c) = return $ ("ok", show $ a+round b+round c)
#endif

runTest :: IO (String, String)
runTest = sumup (1,2,3)
