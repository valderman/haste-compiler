module Tests.PlusPlus where

{-# NOINLINE str1 #-}
str1 = "27"

{-# NOINLINE str2 #-}
str2 = ".0"

{-# NOINLINE theString #-}
theString = str1 ++ str2

runTest :: IO String
runTest = return theString
