{-# LANGUAGE CPP #-}
module Tests.PlusPlusUnicode where
#ifdef __HASTE__
import Haste
output = alert
#else
output = putStrLn
#endif

{-# NOINLINE str1 #-}
str1 = "Tomten klappar händerna"

{-# NOINLINE str2 #-}
str2 = " åt 5001 apor"

{-# NOINLINE theString #-}
theString = str1 ++ str2

runTest :: IO ()
runTest = output theString

