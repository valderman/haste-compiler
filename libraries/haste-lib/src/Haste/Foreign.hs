{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, TypeSynonymInstances,
             FlexibleInstances, TypeFamilies, CPP #-}
-- | Create functions on the fly from JS strings.
--   Slower but more flexible alternative to the standard FFI.
module Haste.Foreign (FFI, Marshal (..), Unpacked, ffi) where
import Haste.Prim
import Haste.JSType
import Data.Word
import Data.Int
import System.IO.Unsafe
import Unsafe.Coerce

#ifdef __HASTE__
foreign import ccall eval :: JSString -> IO (Ptr a)
foreign import ccall "String" jsString :: Double -> JSString
#else
eval :: JSString -> IO (Ptr a)
eval = error "Tried to use eval on server side!"
jsString :: Double -> JSString
jsString = error "Tried to use jsString on server side!"
#endif

-- | Opaque type representing a raw, unpacked JS value. The constructors have
--   no meaning, but are only there to make sure GHC doesn't optimize the low
--   level hackery in this module into oblivion.
data Unpacked = A | B

data Dummy = Dummy Unpacked

-- | Class for marshallable types. Pack takes an opaque JS value and turns it
--   into the type's proper Haste representation, and unpack is its inverse.
--   The default instances make an effort to prevent wrongly typed values
--   through, but you could probably break them with enough creativity.
class Marshal a where
  pack :: Unpacked -> a
  pack = unsafePack

  unpack :: a -> Unpacked
  unpack = unsafeUnpack

instance Marshal Float
instance Marshal Double
instance Marshal JSString where
  pack = jsString . unsafePack
instance Marshal Int where
  pack x = convert (unsafePack x :: Double)
instance Marshal Int8 where
  pack x = convert (unsafePack x :: Double)
instance Marshal Int16 where
  pack x = convert (unsafePack x :: Double)
instance Marshal Int32 where
  pack x = convert (unsafePack x :: Double)
instance Marshal Word where
  pack x = convert (unsafePack x :: Double)
instance Marshal Word8 where
  pack x = convert (unsafePack x :: Double)
instance Marshal Word16 where
  pack x = convert (unsafePack x :: Double)
instance Marshal Word32 where
  pack x = convert (unsafePack x :: Double)
instance Marshal () where
  pack _   = ()
  unpack _ = unpack (0 :: Double)
instance Marshal String where
  pack = fromJSStr . pack
  unpack = unpack . toJSStr
instance Marshal Unpacked where
  pack = id
  unpack = id
instance Marshal Bool where
  unpack True  = jsTrue
  unpack False = jsFalse
  pack x = if pack x > (0 :: Double) then True else False

jsTrue, jsFalse :: Unpacked
jsTrue = unsafePerformIO $ ffi "true"
jsFalse = unsafePerformIO $ ffi "false"

class FFI a where
  type T a
  unpackify :: T a -> a

instance Marshal a => FFI (IO a) where
  type T (IO a) = IO Unpacked
  unpackify = fmap pack

instance (Marshal a, FFI b) => FFI (a -> b) where
  type T (a -> b) = Unpacked -> T b
  unpackify f x = unpackify (f $! unpack x)

-- | Creates a function based on the given string of Javascript code. If this
--   code is not well typed or is otherwise incorrect, your program may crash
--   or misbehave in mystifying ways. Haste makes a best-effort try to save you
--   from poorly typed JS here, but there are no guarantees.
--
--   For instance, the following WILL cause crazy behavior due to wrong types:
--   ffi "(function(x) {return x+1;})" :: Int -> Int -> IO Int
--
--   In other words, this function is completely unsafe - use with caution.
--
--   ALWAYS use type signatures for functions defined using this function, as
--   the argument marshalling is decided by the type signature.
ffi :: FFI a => String -> a
ffi = unpackify . unsafeEval

unsafeUnpack :: a -> Unpacked
unsafeUnpack x =
  case unsafeCoerce x of
    Dummy x' -> x'

unsafePack :: Unpacked -> a
unsafePack = unsafeCoerce . Dummy

unsafeEval :: String -> a
unsafeEval s = unsafePerformIO $ do
  x <- eval (toJSStr s)
  return $ fromPtr x
