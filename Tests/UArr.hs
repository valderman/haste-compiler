module Tests.UArr where
import Data.Array.MArray
import Data.Array.IO
import Data.Int
import Data.Word

runTest :: IO String
runTest = do
  arr <- newArray (0,9) 0 :: IO (IOUArray Int Int)
  writeArray arr 5 999
  a <- readArray arr 4
  b <- readArray arr 5
  c <- readArray arr 6
  writeArray arr 6 (42+a+b+c)
  show `fmap` getElems arr

  arr <- newArray (10,19) 0 :: IO (IOUArray Int8 Double)
  writeArray arr 15 999
  a <- readArray arr 14
  b <- readArray arr 15
  c <- readArray arr 16
  writeArray arr 16 (42+a+b+c)
  show `fmap` getElems arr
