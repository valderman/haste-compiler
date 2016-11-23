{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Haste.Foreign.Array where
import Data.Array.IO hiding (index)
import Data.Array.Unboxed hiding (index)
import Data.Word
import Data.Int
import System.IO.Unsafe
import Haste.Prim
import Haste.Prim.Foreign

-- Additional instances
iouarrToAny :: JSString -> Opaque (IOUArray i e) -> IO JSAny
iouarrToAny = ffi "(function(v,a){return a.d['v'][v];})"

uarrToAny :: JSString -> Opaque (UArray i e) -> IO JSAny
uarrToAny = ffi "(function(v,a){return new a.d['v'][v].constructor(a.d['v'][v]);})"

anyToIOUArr :: JSAny -> IO (Opaque (IOUArray i e))
anyToIOUArr = ffi "(function(a){\
  \var arr = new ByteArray(a['buffer']);\
  \return new T4(0,0,a['length']-1,a['length'],arr);\
  \})"

anyToUArr :: JSAny -> IO (Opaque (UArray i e))
anyToUArr = ffi "(function(a){\
  \var arr = new ByteArray(new a.constructor(a['buffer']));\
  \return new T4(0,0,a['length']-1,a['length'],arr);\
  \})"

class (IArray UArray a, MArray IOUArray a IO) => ArrView a where
  arrView :: a -> JSString

instance ArrView Double where arrView _ = "f64"
instance ArrView Float  where arrView _ = "f32"
instance ArrView Int    where arrView _ = "i32"
instance ArrView Int8   where arrView _ = "i8"
instance ArrView Int16  where arrView _ = "i16"
instance ArrView Int32  where arrView _ = "i32"
instance ArrView Word   where arrView _ = "w32"
instance ArrView Word8  where arrView _ = "w8"
instance ArrView Word16 where arrView _ = "w16"
instance ArrView Word32 where arrView _ = "w32"

instance ArrView e => FromAny (IOUArray i e) where
  fromAny = fmap fromOpaque . anyToIOUArr

instance forall i e. ArrView e => ToAny (IOUArray i e) where
  toAny = unsafePerformIO . iouarrToAny (arrView (undefined :: e)) . toOpaque

instance (Ix i, ArrView e) => FromAny (UArray i e) where
  fromAny = fmap fromOpaque . anyToUArr

instance forall i e. (Ix i, ArrView e) => ToAny (UArray i e) where
  toAny = unsafePerformIO . uarrToAny (arrView (undefined :: e)) . toOpaque
