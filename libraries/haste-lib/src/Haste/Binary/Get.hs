{-# Language CPP, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Haste.Binary.Get (
    Get,
    getWord8, getWord16le, getWord32le,
    getInt8, getInt16le, getInt32le,
    getFloat32le, getFloat64le,
    getBytes, getJSString, skip,
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
import Data.Char (chr)
import qualified Data.Binary as B
import qualified Data.Binary.IEEE754 as BI
import qualified Data.Binary.Get as BG
import qualified Control.Exception as Ex
#endif

#ifdef __HASTE__
data Result a = Ok !Int !a | Fail !String
data Get a = Get {unG :: JSAny -> Int -> Result a}

instance Functor Get where
  fmap f (Get m) = Get $ \buf next ->
    case m buf next of
      Ok next' x -> Ok next' (f x)
      Fail s     -> Fail s

instance Applicative Get where
  (<*>) = ap
  pure  = return

instance Monad Get where
  return x = Get $ \_ next -> Ok next x
  (Get m) >>= f = Get $ \buf next ->
    case m buf next of
      Ok next' x -> unG (f x) buf next'
      Fail e     -> Fail e
  fail s = Get $ \_ _ -> Fail s

getW8 :: JSAny -> Int -> IO Word8
getW8 = ffi "(function(b,i){return b.getUint8(i);})"

getWord8 :: Get Word8
getWord8 =
  Get $ \buf next -> Ok (next+1) (unsafePerformIO $ getW8 buf next)

getW16le :: JSAny -> Int -> IO Word16
getW16le = ffi "(function(b,i){return b.getUint16(i,true);})"

getWord16le :: Get Word16
getWord16le =
  Get $ \buf next -> Ok (next+2) (unsafePerformIO $ getW16le buf next)

getW32le :: JSAny -> Int -> IO Word32
getW32le = ffi "(function(b,i){return b.getUint32(i,true);})"

getWord32le :: Get Word32
getWord32le =
  Get $ \buf next -> Ok (next+4) (unsafePerformIO $ getW32le buf next)

getI8 :: JSAny -> Int -> IO Int8
getI8 = ffi "(function(b,i){return b.getInt8(i);})"

getInt8 :: Get Int8
getInt8 =
  Get $ \buf next -> Ok (next+1) (unsafePerformIO $ getI8 buf next)

getI16le :: JSAny -> Int -> IO Int16
getI16le = ffi "(function(b,i){return b.getInt16(i,true);})"

getInt16le :: Get Int16
getInt16le =
  Get $ \buf next -> Ok (next+2) (unsafePerformIO $ getI16le buf next)

getI32le :: JSAny -> Int -> IO Int32
getI32le = ffi "(function(b,i){return b.getInt32(i,true);})"

getInt32le :: Get Int32
getInt32le =
  Get $ \buf next -> Ok (next+4) (unsafePerformIO $ getI32le buf next)

getF32le :: JSAny -> Int -> IO Float
getF32le = ffi "(function(b,i){return b.getFloat32(i,true);})"

getFloat32le :: Get Float
getFloat32le =
  Get $ \buf next -> Ok (next+4) (unsafePerformIO $ getF32le buf next)

getF64le :: JSAny -> Int -> IO Double
getF64le = ffi "(function(b,i){return b.getFloat64(i,true);})"

getFloat64le :: Get Double
getFloat64le =
  Get $ \buf next -> Ok (next+8) (unsafePerformIO $ getF64le buf next)

getBytes :: Int -> Get BlobData
getBytes len = Get $ \buf next -> Ok (next+len) (BlobData next len buf)

-- | Read a 'JSString' of @n@ characters. Encoding is assumed to be UTF-16.
getJSString :: Word32 -> Get JSString
getJSString len = Get $ \buf next ->
  Ok (next+fromIntegral (len+len)) (unsafePerformIO $ getJSS buf next len)

getJSS :: JSAny -> Int -> Word32 -> IO JSString
getJSS = ffi "(function(b,off,len){return String.fromCharCode.apply(null,new Uint16Array(b.buffer,off,len));})"

-- | Skip n bytes of input.
skip :: Int -> Get ()
skip len = Get $ \_buf next -> Ok (next+len) ()

-- | Run a Get computation.
runGet :: Get a -> BlobData -> Either String a
runGet (Get p) (BlobData off len bd) = do
  case p bd off of
    Ok consumed x
      | consumed <= len -> Right x
      | otherwise       -> Left "Not enough data!"
    Fail s              -> Left s

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

getJSString :: Int -> Get JSString
getJSString len = Get $ do
  toJSStr `fmap` forM [1..len] (\_ -> fmap (chr . fromIntegral) BG.getWord16le)

-- | Skip n bytes of input.
skip :: Int -> Get ()
skip = Get . BG.skip

#endif
