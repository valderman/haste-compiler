module Tests.Arr where
import Data.Array.MArray
import Data.Array.IO

runTest :: IO [Int]
runTest = do
  arr <- newArray (0,9) (0::Int) :: IO (IOArray Int Int)
  writeArray arr 5 999
  a <- readArray arr 4
  b <- readArray arr 5
  c <- readArray arr 6
  writeArray arr 6 (42+a+b+c)
  getElems arr
