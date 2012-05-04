module Tests.BitOps where
import Data.Word
import Data.Bits

{-# NOINLINE num #-}
num :: Word
num = 0xffffffff

runTest :: IO String
runTest = return $ show (num .&. num)
