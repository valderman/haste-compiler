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
