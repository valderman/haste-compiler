module Tests.RoundToInteger where

r :: [Integer]
r = map round [-4, -3.9 .. 4]

runTest = putStrLn $ show r
