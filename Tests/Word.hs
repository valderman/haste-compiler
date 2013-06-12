{-# LANGUAGE CPP #-}
module Tests.Word where
import Data.Word
import Data.Int

#ifdef __HASTE__
import Haste
type I = Int
type W = Word
i2w = convert
w2i = convert
showish = toString
#else
type I = Int32
type W = Word32
i2w = fromIntegral
w2i = fromIntegral
showish = show
#endif

i2w :: I -> W
w2i :: W -> I

{-# NOINLINE a #-}
a :: I
a = -15

{-# NOINLINE b #-}
b :: I
b = 27

{-# NOINLINE c #-}
c :: W
c = 97

runTest :: IO [String]
runTest = return [showish $ i2w a,
                  showish $ i2w b,
                  showish $ i2w $ w2i c]
