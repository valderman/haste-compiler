{-# LANGUAGE CPP #-}
module Tests.Sumlist where

mySum :: Num a => [a] -> a
mySum (x:xs) = x+mySum xs
mySum _      = 0

runTest :: IO [String]
runTest = return [show $ mySum [1::Int .. 10],
                  show $ mySum [1::Integer .. 10],
                  show $ mySum [1::Float .. 10],
                  show $ mySum [1::Double .. 10]]
