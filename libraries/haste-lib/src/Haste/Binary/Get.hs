{-# Language CPP, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Haste.Binary.Get (
    Get,
    getWord8, getWord16le, getWord32le,
    getInt8, getInt16le, getInt32le,
    getFloat32le, getFloat64le,
    getBytes, skip,
    runGet
  ) where
import Data.Int
import Data.Word
import Haste.Prim
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

{-# NOINLINE getW8 #-}
getW8 :: Unpacked -> Int -> IO Word8
getW8 = ffi "(function(b,i){return b.getUint8(i);})"

getWord8 :: Get Word8
getWord8 =
  Get $ \buf next -> Right (next+1, unsafePerformIO $ getW8 buf next)

{-# NOINLINE getW16le #-}
getW16le :: Unpacked -> Int -> IO Word16
getW16le = ffi "(function(b,i){return b.getUint16(i,true);})"

getWord16le :: Get Word16
getWord16le =
  Get $ \buf next -> Right (next+2, unsafePerformIO $ getW16le buf next)

{-# NOINLINE getW32le #-}
getW32le :: Unpacked -> Int -> IO Word32
getW32le = ffi "(function(b,i){return b.getUint32(i,true);})"

getWord32le :: Get Word32
getWord32le =
  Get $ \buf next -> Right (next+4, unsafePerformIO $ getW32le buf next)

{-# NOINLINE getI8 #-}
getI8 :: Unpacked -> Int -> IO Int8
getI8 = ffi "(function(b,i){return b.getInt8(i);})"

getInt8 :: Get Int8
getInt8 =
  Get $ \buf next -> Right (next+1, unsafePerformIO $ getI8 buf next)

{-# NOINLINE getI16le #-}
getI16le :: Unpacked -> Int -> IO Int16
getI16le = ffi "(function(b,i){return b.getInt16(i,true);})"

getInt16le :: Get Int16
getInt16le =
  Get $ \buf next -> Right (next+2, unsafePerformIO $ getI16le buf next)

{-# NOINLINE getI32le #-}
getI32le :: Unpacked -> Int -> IO Int32
getI32le = ffi "(function(b,i){return b.getInt32(i,true);})"

getInt32le :: Get Int32
getInt32le =
  Get $ \buf next -> Right (next+4, unsafePerformIO $ getI32le buf next)

{-# NOINLINE getF32le #-}
getF32le :: Unpacked -> Int -> IO Float
getF32le = ffi "(function(b,i){return b.getFloat32(i,true);})"

getFloat32le :: Get Float
getFloat32le =
  Get $ \buf next -> Right (next+4, unsafePerformIO $ getF32le buf next)

{-# NOINLINE getF64le #-}
getF64le :: Unpacked -> Int -> IO Double
getF64le = ffi "(function(b,i){return b.getFloat64(i,true);})"

getFloat64le :: Get Double
getFloat64le =
  Get $ \buf next -> Right (next+8, unsafePerformIO $ getF64le buf next)

getBytes :: Int -> Get BlobData
getBytes len = Get $ \buf next -> Right (next+len, BlobData next len buf)

-- | Skip n bytes of input.
skip :: Int -> Get ()
skip len = Get $ \buf next -> Right (next+len, ())

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

-- | Skip n bytes of input.
skip :: Int -> Get ()
skip = Get . BG.skip

#endif
