{-# LANGUAGE CPP #-}
module Tests.EscapedChars where
#ifdef HASTE
import Haste
output = alert
#else
output = putStrLn
#endif

{-# NOINLINE str1 #-}
str1 = "Tomten klappar \"händerna\""

{-# NOINLINE str2 #-}
str2 = " åt:\t5001 apor med backslashar: \\"

{-# NOINLINE theString #-}
theString = str1 ++ str2

runTest :: IO ()
runTest = output theString

