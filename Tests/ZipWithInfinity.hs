module Tests.ZipWithInfinity where
import Haste

nums :: [Int]
nums = foldr (zipWith (+)) (repeat 0) [[0,1],[1,0]]

runTest :: IO ()
runTest = putStrLn $ show nums
