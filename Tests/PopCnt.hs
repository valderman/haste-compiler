module Tests.PopCnt where
import Data.Bits
import Data.Int
import Data.Word

{-# NOINLINE num1 #-}
num1 :: Int
num1 = 403834

{-# NOINLINE num2 #-}
num2 :: Int64
num2 = -213423450934

{-# NOINLINE num3 #-}
num3 :: Word
num3 = 50000

{-# NOINLINE num4 #-}
num4 :: Word64
num4 = fromIntegral num2*495+2934

runTest :: IO [Int]
runTest = return $ [popCount num1,
                    popCount num2,
                    popCount num3,
                    popCount num4]
