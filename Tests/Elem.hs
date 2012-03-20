module Tests.Elem where

{-# NOINLINE str1 #-}
str1 = "Tomten klappar händerna."

{-# NOINLINE str2 #-}
str2 = "5001 apor"

runTest :: IO (Bool, Bool)
runTest = return (elem '.' str1, not $ elem '.' str2)
