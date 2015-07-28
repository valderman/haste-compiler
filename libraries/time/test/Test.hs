module Main where
import Test.Framework
import Test.Tests
import Foreign.C.Types

main :: IO ()
main = do
  if (toRational (1000000000000 :: CTime)) /= (1000000000000 :: Rational)
    then putStrLn "WARNING: Some tests will incorrectly fail due to a 32-bit time_t C type."
    else return ()
  defaultMain tests
