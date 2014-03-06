{-# Language CPP, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Haste.Binary.Get (
    Get,
    getWord8, getWord16le, getWord32le,
    getInt8, getInt16le, getInt32le,
    getFloat32le, getFloat64le,
    getBytes,
    runGet
  ) where
import Data.Int
import Data.Word
import Haste
import Haste.Foreign
import Haste.Binary.Types
import Control.Applicative
import Control.Monad
import System.IO.Unsafe
#ifndef __HASTE__
import qualified Data.Binary as B
import qualified Data.Binary.IEEE754 as BI
import qualified Data.Binary.Get as BG
import qualified Control.Exception as Ex
#endif

#ifdef __HASTE__
data Get a = Get {unG :: Unpacked -> Int -> Either String (Int, a)}

instance Functor Get where
  fmap f (Get m) = Get $ \buf next -> fmap (fmap f) (m buf next)

instance Applicative Get where
  (<*>) = ap
  pure  = return

instance Monad Get where
  return x = Get $ \_ next -> Right (next, x)
  (Get m) >>= f = Get $ \buf next ->
    case m buf next of
      Right (next', x) -> unG (f x) buf next'
      Left e           -> Left e
  fail s = Get $ \_ _ -> Left s

getWord8 :: Get Word8
getWord8 = Get $ \buf next -> Right (next+1, getB buf next)
  where
    getB b n = unsafePerformIO $ ffi "(function(b,i){return b.getUint8(i,true);})" b n

getWord16le :: Get Word16
getWord16le = Get $ \buf next -> Right (next+2, getW buf next)
  where
    getW b n = unsafePerformIO $ ffi "(function(b,i){return b.getUint16(i,true);})" b n

getWord32le :: Get Word32
getWord32le = Get $ \buf next -> Right (next+4, getW buf next)
  where
    getW b n = unsafePerformIO $ ffi "(function(b,i){return b.getUint32(i,true);})" b n

getInt8 :: Get Int8
getInt8 = Get $ \buf next -> Right (next+1, getB buf next)
  where
    getB b n = unsafePerformIO $ ffi "(function(b,i){return b.getInt8(i,true);})" b n

getInt16le :: Get Int16
getInt16le = Get $ \buf next -> Right (next+2, getW buf next)
  where
    getW b n = unsafePerformIO $ ffi "(function(b,i){return b.getInt16(i,true);})" b n

getInt32le :: Get Int32
getInt32le = Get $ \buf next -> Right (next+4, getW buf next)
  where
    getW b n = unsafePerformIO $ ffi "(function(b,i){return b.getInt32(i,true);})" b n

getFloat32le :: Get Float
getFloat32le = Get $ \buf next -> Right (next+4, getF buf next)
  where
    getF b n = unsafePerformIO $ ffi "(function(b,i){return b.getFloat32(i,true);})" b n

getFloat64le :: Get Double
getFloat64le = Get $ \buf next -> Right (next+8, getF buf next)
  where
    getF b n = unsafePerformIO $ ffi "(function(b,i){return b.getFloat64(i,true);})" b n

getBytes :: Int -> Get BlobData
getBytes len = Get $ \buf next -> Right (next+len, BlobData next len buf)

-- | Run a Get computation.
runGet :: Get a -> BlobData -> Either String a
runGet (Get p) (BlobData off len bd) = do
    (consumed, x) <- p bd off
    if consumed <= len
      then Right x
      else Left "Not enough data!"

#else

newtype Get a = Get (BG.Get a) deriving (Functor, Applicative, Monad)

runGet :: Get a -> BlobData -> Either String a
runGet (Get g) (BlobData bd) = unsafePerformIO $ do
  Ex.catch (Right <$> (return $! BG.runGet g bd)) mEx

mEx :: Ex.SomeException -> IO (Either String a)
mEx ex = return . Left $ show ex

getWord8 :: Get Word8
getWord8 = Get BG.getWord8

getWord16le :: Get Word16
getWord16le = Get BG.getWord16le

getWord32le :: Get Word32
getWord32le = Get BG.getWord32le

getInt8 :: Get Int8
getInt8 = Get B.get

getInt16le :: Get Int16
getInt16le = fromIntegral <$> getWord16le

getInt32le :: Get Int32
getInt32le = fromIntegral <$> getWord32le

getFloat32le :: Get Float
getFloat32le = Get BI.getFloat32le

getFloat64le :: Get Double
getFloat64le = Get BI.getFloat64le

getBytes :: Int -> Get BlobData
getBytes len = Get $ do
  bs <- BG.getLazyByteString (fromIntegral len)
  return (BlobData bs)

#endif
