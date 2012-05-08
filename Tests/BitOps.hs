module Tests.BitOps where
import Data.Word
import Data.Bits

{-# NOINLINE num #-}
num :: Word
num = 0xffffffff

{-# NOINLINE int #-}
int :: Int
int = -1

runTest :: IO [String]
runTest = return [
  show (num .&. num),
  show (num .|. num),
  show (num `xor` num),
  show ((num .&. 0xffff) `shiftL` 2),
  show (num `shiftR` 3),
  show (int .&. int),
  show (int .|. 4531),
  show (int `xor` 55423),
  show (int `shiftR` 1),
  show ((int .&. 0xffff) `shiftL` 2)]
