module Tests.PlusPlusUnicode where

{-# NOINLINE str1 #-}
str1 = "Tomten klappar händerna"

{-# NOINLINE str2 #-}
str2 = " åt 5001 apor"

{-# NOINLINE theString #-}
theString = str1 ++ str2

runTest :: IO String
runTest = return theString
