{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, MagicHash, 
    TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module Haste.Prim (JSString, toJSStr, fromJSStr, NumberRep (..), JSAny,
                   Ptr, toPtr, fromPtr) where
import Foreign.Ptr
import Unsafe.Coerce
import GHC.CString
import GHC.Types
import GHC.Prim
import GHC.Word
import Data.String
import Data.Int (Int32)

foreign import ccall _str :: JSString
foreign import ccall strEq :: JSString -> JSString -> Bool
foreign import ccall strOrd :: JSString -> JSString -> Ptr Ordering

-- | "Pointers" need to be wrapped in a data constructor.
data FakePtr a = FakePtr a

type JSAny = Ptr Haste.Prim.Any
data Any
type JSString = Ptr JSChr
data JSChr

instance Eq JSString where
  (==) = strEq

instance Ord JSString where
  compare a b = fromPtr (strOrd a b)

-- | In normal Haskell, we use Storable for data that can be pointed to. When
--   we compile to JS, however, anything can be "pointed" to and nothing needs
--   to be stored.
toPtr :: a -> Ptr a
toPtr = unsafeCoerce . FakePtr

-- | Unwrap a "pointer" to something.
fromPtr :: Ptr a -> a
fromPtr ptr =
  case unsafeCoerce ptr of
    FakePtr val -> val

{-# RULES "toJSS/fromJSS" forall s. toJSStr (fromJSStr s) = s #-}
{-# RULES "fromJSS/toJSS" forall s. fromJSStr (toJSStr s) = s #-}
{-# RULES "toJSS/unCSTR" forall s. toJSStr (unpackCString# s) = toPtr (unsafeCoerce# s) #-}
{-# RULES "toJSS/unCSTRU8" forall s. toJSStr (unpackCStringUtf8# s) = toPtr (unsafeCoerce# s) #-}

{-# NOINLINE toJSStr #-}
-- | Defined in lib/rts.js
toJSStr :: String -> JSString
toJSStr s = s `seq` _str

instance IsString JSString where
  fromString = toJSStr

{-# NOINLINE fromJSStr #-}
-- | Defined in lib/rts.js
fromJSStr :: JSString -> String
fromJSStr s = s `seq` []

class NumberRep a where
  -- | Convert any type with a Number representation to a 32-bit signed integer.
  round_ :: a -> Int
  -- | @'floor_' x@ converts any type with a Number representation to the greatest 32-bit signed integer not greater than @x@
  floor_ :: a -> Int
  -- | @'ceiling_' x@ converts any type with a Number representation to the least 32-bit signed integer not lesser than @x@
  ceiling_ :: a -> Int
  -- | Create any type with a Number representation from a 32-bit signed integer.
  fromInt :: Int -> a
  fromInt = unsafeCoerce#

instance NumberRep Double where
  round_ (D# d) = I# (jsRound d)
  floor_ (D# d) = I# (jsFloor d)
  ceiling_ (D# d) = I# (jsCeiling d)
instance NumberRep Float where
  round_ (F# f) = I# (unsafeCoerce# (jsRound (unsafeCoerce# f)))
  floor_ (F# f) = I# (unsafeCoerce# (jsFloor (unsafeCoerce# f)))
  ceiling_ (F# f) = I# (unsafeCoerce# (jsCeiling (unsafeCoerce# f)))
instance NumberRep Int where
  round_ = id
  floor_ = id
  ceiling_ = id
instance NumberRep Int32 where
  round_ = unsafeCoerce#
  floor_ = unsafeCoerce#
  ceiling_ = unsafeCoerce#
instance NumberRep Word where
  round_ (W# w) = I# (word2Int# w)
  floor_ (W# w) = I# (word2Int# w)
  ceiling_ (W# w) = I# (word2Int# w)
  fromInt (I# i) = W# (int2Word# i)
instance NumberRep Word32 where
  round_ (W32# w) = I# (word2Int# (unsafeCoerce# w))
  floor_ (W32# w) = I# (word2Int# (unsafeCoerce# w))
  ceiling_ (W32# w) = I# (word2Int# (unsafeCoerce# w))
  fromInt (I# i) = W32# (int2Word# i)

{-# NOINLINE jsRound #-}
-- | Defined in lib/rts.js
jsRound :: Double# -> Int#
jsRound x = unsafeCoerce# x

{-# NOINLINE jsFloor #-}
-- | Defined in lib/rts.js
jsFloor :: Double# -> Int#
jsFloor x = unsafeCoerce# x

{-# NOINLINE jsCeiling #-}
-- | Defined in lib/rts.js
jsCeiling :: Double# -> Int#
jsCeiling x = unsafeCoerce# x
