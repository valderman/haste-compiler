{-# LANGUAGE CPP #-}
module Tests.EscapedChars where
import Data.Char (ord)

{-# NOINLINE str1 #-}
str1 = "Tomten klappar \"händerna\""

{-# NOINLINE str2 #-}
str2 = " åt:\t5001 apor med backslashar: \\"

{-# NOINLINE theString #-}
theString = str1 ++ str2

-- Standalone SpiderMonkey can't handle showing Unicode chars, so we have to
-- look at the char codes. :(
runTest :: IO [Int]
runTest = return $ map ord theString
