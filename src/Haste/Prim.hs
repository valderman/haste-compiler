{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, MagicHash #-}
module Haste.Prim (JSString, toJSStr, fromJSStr, round_, NumberRep) where
import Foreign.Ptr
import GHC.Types
import GHC.Prim

foreign import ccall _str :: JSString

type JSString = Ptr JSChr
data JSChr

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
jsRound :: Double# -> Int#
jsRound x = unsafeCoerce# x
