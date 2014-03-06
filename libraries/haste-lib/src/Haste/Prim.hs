{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, MagicHash, 
    TypeSynonymInstances, FlexibleInstances, OverlappingInstances, CPP #-}
module Haste.Prim (JSString, URL, toJSStr, fromJSStr, catJSStr, JSAny,
                   Ptr, toPtr, fromPtr) where
import Foreign.Ptr
import Data.String
#ifdef __HASTE__
import Unsafe.Coerce
import GHC.CString
import GHC.Prim
import qualified GHC.HastePrim as HP
#else
import Data.List (intercalate)
#endif

type URL = String
type JSAny = Ptr Haste.Prim.Any
data Any

-- | Concatenate a series of JSStrings using the specified separator.
catJSStr :: JSString -> [JSString] -> JSString
#ifdef __HASTE__
foreign import ccall jsCat    :: Ptr [JSString] -> JSString -> JSString
catJSStr sep strs = jsCat (toPtr strs) sep
#else
catJSStr sep strs = toJSStr $ intercalate (fromJSStr sep) (map fromJSStr strs)
#endif

#ifdef __HASTE__
foreign import ccall strEq :: JSString -> JSString -> Bool
foreign import ccall strOrd :: JSString -> JSString -> Ptr Ordering

-- | "Pointers" need to be wrapped in a data constructor.
data FakePtr a = FakePtr a

type JSString = Ptr JSChr
data JSChr

instance Eq JSString where
  (==) = strEq

instance Ord JSString where
  compare a b = fromPtr (strOrd a b)

instance Show JSString where
  show = fromJSStr

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

toJSStr :: String -> JSString
toJSStr = unsafeCoerce# HP.toJSStr

instance IsString JSString where
  fromString = toJSStr

fromJSStr :: JSString -> String
fromJSStr = unsafeCoerce# HP.fromJSStr

#else

-- | JSStrings are represented as normal strings server-side; should probably
--   be changed to ByteString or Text.
newtype JSString = JSString String

instance IsString JSString where
  fromString = JSString

instance Eq JSString where
  (JSString a) == (JSString b) = a == b

instance Ord JSString where
  (JSString a) `compare` (JSString b) = a `compare` b

instance Show JSString where
  show = fromJSStr

toJSStr :: String -> JSString
toJSStr = JSString

fromJSStr :: JSString -> String
fromJSStr (JSString s) = s

toPtr :: a -> Ptr a
toPtr = error "toPtr used in native code!"

fromPtr :: Ptr a -> a
fromPtr = error "fromPtr used in native code!"

#endif
