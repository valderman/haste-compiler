{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, MagicHash, 
    TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module Haste.Prim (JSString, toJSStr, fromJSStr, NumberRep (..), JSAny,
                   mkPtr, unsafeUnPtr) where
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

type JSAny = Ptr Haste.Prim.Any
data Any
type JSString = Ptr JSChr
data JSChr

instance Eq JSString where
  (==) = strEq

instance Ord JSString where
  compare a b = unsafeUnPtr (strOrd a b)

-- | We need to fake the same representation as Ptr a; FakePtr does this.
data FakePtr a = FakePtr a

-- | In normal Haskell, we use Storable for data that can be pointed to. When
--   we compile to JS, however, anything can be "pointed" to and nothing needs
--   to be stored, so we just wrap a value in another constructor and cast it
--   to a pointer.
mkPtr :: a -> Ptr a
mkPtr = unsafeCoerce . FakePtr

-- | Unsafely read a value from a pointer. This is only safe if the value
--   pointed to is immutable; even then, please don't do it.
unsafeUnPtr :: Ptr a -> a
unsafeUnPtr p =
  case unsafeCoerce p of
    FakePtr x -> x

{-# RULES "toJSS/fromJSS" forall s. toJSStr (fromJSStr s) = s #-}
{-# RULES "fromJSS/toJSS" forall s. fromJSStr (toJSStr s) = s #-}
{-# RULES "toJSS/unCSTR" forall s. toJSStr (unpackCString# s) = mkPtr (unsafeCoerce# s) #-}
{-# RULES "toJSS/unCSTRU8" forall s. toJSStr (unpackCStringUtf8# s) = mkPtr (unsafeCoerce# s) #-}

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
  round_  :: a -> Int
  fromInt :: Int -> a
  fromInt = unsafeCoerce#
  
instance NumberRep Double where
  round_ (D# d) = I# (jsRound d)
instance NumberRep Float where
  round_ (F# f) = I# (unsafeCoerce# (jsRound (unsafeCoerce# f)))
instance NumberRep Int where
  round_ = id
instance NumberRep Int32 where
  round_ = unsafeCoerce#
instance NumberRep Word where
  round_ (W# w) = I# (word2Int# w)
  fromInt (I# i) = W# (int2Word# i)
instance NumberRep Word32 where
  round_ (W32# w) = I# (word2Int# (unsafeCoerce# w))
  fromInt (I# i) = W32# (int2Word# i)

{-# NOINLINE jsRound #-}
-- | Defined in lib/rts.js
jsRound :: Double# -> Int#
jsRound x = unsafeCoerce# x
