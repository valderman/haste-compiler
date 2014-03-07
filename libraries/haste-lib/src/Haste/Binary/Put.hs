{-# LANGUAGE CPP, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Haste.Binary.Put (
    Put, PutM,
    putWord8, putWord16le, putWord32le,
    putInt8, putInt16le, putInt32le,
    putFloat32le, putFloat64le,
    putBlob,
    runPut
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
import qualified Data.Binary.Put as BP
import qualified Data.Binary.Put as BP
#endif

type Put = PutM ()

#ifdef __HASTE__
type JSArr = Unpacked
newArr :: IO JSArr
newArr = ffi "(function(){return [];})"

push :: Marshal a => JSArr -> a -> IO ()
push = ffi "(function(a,x) {a.push(x);})"

data PutM a = PutM {unP :: JSArr -> IO a}

instance Functor PutM where
  fmap f (PutM m) = PutM $ \a -> fmap f (m a)

instance Applicative PutM where
  (<*>) = ap
  pure  = return

instance Monad PutM where
  return x = PutM $ \_ -> return x
  PutM m >>= f = PutM $ \a -> do
    x <- m a
    unP (f x) a

putWord8 :: Word8 -> Put
putWord8 w = PutM $ \a -> push a (toAB "Uint8Array" 1 w)

putWord16le :: Word16 -> Put
putWord16le w = PutM $ \a -> push a (toAB "Uint16Array" 2 w)

putWord32le :: Word32 -> Put
putWord32le w = PutM $ \a -> push a (toAB "Uint32Array" 4 w)

putInt8 :: Int8 -> Put
putInt8 i = PutM $ \a -> push a (toAB "Int8Array" 1 i)

putInt16le :: Int16 -> Put
putInt16le i = PutM $ \a -> push a (toAB "Int16Array" 2 i)

putInt32le :: Int32 -> Put
putInt32le i = PutM $ \a -> push a (toAB "Int32Array" 4 i)

putFloat32le :: Float -> Put
putFloat32le f = PutM $ \a -> push a (unsafePerformIO $ f2ab f)

{-# NOINLINE f2ab #-}
f2ab :: Float -> IO Unpacked
f2ab = ffi "(function(f) {var a=new ArrayBuffer(4);new DataView(a).setFloat32(0,f,true);return a;})"

putFloat64le :: Double -> Put
putFloat64le f = PutM $ \a -> push a (unsafePerformIO $ d2ab f)

{-# NOINLINE d2ab #-}
d2ab :: Double -> IO Unpacked
d2ab = ffi "(function(f) {var a=new ArrayBuffer(8);new DataView(a).setFloat64(0,f,true);return a;})"

-- | Write a Blob verbatim into the output stream.
putBlob :: Blob -> Put
putBlob b = PutM $ \a -> push a (unpack b)

toAB :: Marshal a => JSString -> Int -> a -> Unpacked
toAB view size elem = unsafePerformIO $ toABle view size (unpack elem)

{-# NOINLINE toABle #-}
toABle :: Marshal a => JSString -> Int -> a -> IO Unpacked
toABle = ffi "window['toABle']"

-- | Run a Put computation.
runPut :: Put -> Blob
runPut (PutM putEverything) = unsafePerformIO $ do
    a <- newArr
    putEverything a
    go a
  where
    go :: JSArr -> IO Blob
    go = ffi "(function(parts){return new Blob(parts);})"

#else

newtype PutM a = PutM (BP.PutM a) deriving (Functor, Applicative, Monad)

runPut :: Put -> Blob
runPut (PutM p) = Blob (BP.runPut p)

putWord8 :: Word8 -> Put
putWord8 = PutM . BP.putWord8

putWord16le :: Word16 -> Put
putWord16le = PutM . BP.putWord16le

putWord32le :: Word32 -> Put
putWord32le = PutM . BP.putWord32le

putInt8 :: Int8 -> Put
putInt8 = PutM . B.put

putInt16le :: Int16 -> Put
putInt16le = putWord16le . fromIntegral

putInt32le :: Int32 -> Put
putInt32le = putWord32le . fromIntegral

putFloat32le :: Float -> Put
putFloat32le = PutM . BI.putFloat32le

putFloat64le :: Double -> Put
putFloat64le = PutM . BI.putFloat64le

putBlob :: Blob -> Put
putBlob (Blob b) = PutM $ BP.putLazyByteString b

#endif
