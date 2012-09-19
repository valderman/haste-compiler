{-# LANGUAGE CPP #-}
module Tests.Sumlist where
#ifdef __HASTE__
import Haste
str :: Showable a => a -> String
str = show_
#else
str :: Show a => a -> String
str = show
#endif

mySum :: Num a => [a] -> a
mySum (x:xs) = x+mySum xs
mySum _      = 0

runTest :: IO [String]
runTest = return [str  $ mySum [1::Int .. 10],
                  show $ mySum [1::Integer .. 10],
                  str  $ mySum [1::Float .. 10],
                  str  $ mySum [1::Double .. 10]]
