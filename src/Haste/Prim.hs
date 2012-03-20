{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, MagicHash #-}
module Haste.Prim (JSString, toJSStr, fromJSStr, round_, NumberRep,
                   JSAny, mkPtr) where
import Foreign.Ptr
import Unsafe.Coerce
import GHC.Types
import GHC.Prim

foreign import ccall _str :: JSString

type JSAny = Ptr Haste.Prim.Any
data Any
type JSString = Ptr JSChr
data JSChr

-- | We need to fake the same representation as Ptr a; FakePtr does this.
data FakePtr a = FakePtr a

-- | In normal Haskell, we use Storable for data that can be pointed to. When
--   we compile to JS, however, anything can be "pointed" to and nothing needs
--   to be stored, so we just wrap a value in another constructor and cast it
--   to a pointer.
mkPtr :: a -> Ptr a
mkPtr = unsafeCoerce . FakePtr

{-# NOINLINE toJSStr #-}
-- | Defined in lib/rts.js
toJSStr :: String -> JSString
toJSStr s = s `seq` _str

{-# NOINLINE fromJSStr #-}
-- | Defined in lib/rts.js
fromJSStr :: JSString -> String
fromJSStr s = s `seq` []

class NumberRep a where
  round_ :: a -> Int
  
instance NumberRep Double where
  round_ (D# d) = I# (jsRound d)
instance NumberRep Float where
  round_ (F# f) = I# (unsafeCoerce# (jsRound (unsafeCoerce# f)))
instance NumberRep Int where
  round_ = id

{-# NOINLINE jsRound #-}
-- | Defined in lib/rts.js
jsRound :: Double# -> Int#
jsRound x = unsafeCoerce# x
