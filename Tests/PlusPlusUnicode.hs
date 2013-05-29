{-# LANGUAGE CPP #-}
module Tests.PlusPlusUnicode where
import Data.Char (ord)

{-# NOINLINE str1 #-}
str1 = "Tomten klappar händerna"

{-# NOINLINE str2 #-}
str2 = " åt 5001 apor"

{-# NOINLINE theString #-}
theString = str1 ++ str2

-- Displaying unicode chars is kind of broken in the standalone SpiderMonkey
-- interpreter, so we'll have to look at the char codes. :(
runTest :: IO [Int]
runTest = return $ map ord theString
