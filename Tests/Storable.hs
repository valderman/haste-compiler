module Tests.Storable where
import Foreign
import Data.Word
import Data.Int

test :: (Show a, Storable a) => a -> IO String
test val = do
  ptr <- mallocForeignPtr
  withForeignPtr ptr $ flip poke val
  firstByte <- withForeignPtr (castForeignPtr ptr) peek
  value <- withForeignPtr ptr peek
  return $ show (firstByte :: Word8, value)

runTest = do
 sequence
   [ test (0x0123456789abcdef :: Word64)
   , test (0xaabbccdd :: Word32)
   , test (123456789000000005 :: Int64)
   , test (-15 :: Int64)
   , test (3985394 :: Int32)
   , test (-23423 :: Int32)
   , test (123.456 :: Double)
   ]
