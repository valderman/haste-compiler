{-# LANGUAGE CPP #-}
module Tests.Word where
import Data.Word
#ifdef HASTE
import Haste
i2w = fromInt
w2i = round_
#else
i2w = fromIntegral
w2i = fromIntegral
#endif

{-# NOINLINE a #-}
a :: Int
a = -15

{-# NOINLINE b #-}
b :: Int
b = 27

{-# NOINLINE c #-}
c :: Word32
c = 97

runTest :: IO [Word32]
runTest = return [i2w a, i2w b, i2w $ w2i c]
