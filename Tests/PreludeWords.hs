module Tests.PreludeWords where

{-# NOINLINE text #-}
text :: String
text = "internet, lol, internet"

runTest :: IO Int
runTest = return . length $ words text
