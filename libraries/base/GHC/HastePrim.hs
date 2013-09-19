{-# LANGUAGE ForeignFunctionInterface,
             UnliftedFFITypes,
             GHCForeignImportPrim, 
             NoImplicitPrelude, 
             MagicHash #-}
-- | Haste primitives used in various places in the haxxored base.
module GHC.HastePrim (
    JSString, jsShowD, jsShowF, jsShowW, jsShowI,
    fromJSStr, toJSStr, fromJSStr#, toJSStr#
  ) where
import GHC.Prim
import GHC.Base

-- Some imports for Haste
type JSString = ByteArray#
type Dontcare# = ByteArray#
foreign import ccall "jsShow"   jsShowD# :: Double -> JSString
foreign import ccall "jsShow"   jsShowF# :: Float -> JSString
foreign import ccall "jsShowI"  jsShowW# :: Word -> JSString
foreign import ccall "jsShowI"  jsShowI# :: Int -> JSString
foreign import prim "fromJSStr" fromJSStr# :: JSString -> Dontcare#
foreign import prim "toJSStr"   toJSStr# :: Dontcare# -> JSString

-- fromJSStr# returns [Char], but FFI won't let us do that, so...
fromJSStr :: JSString -> String
fromJSStr s =
  case unsafeCoerce# s of
    I# s' -> (unsafeCoerce# (fromJSStr# (unsafeCoerce# s')))

toJSStr :: String -> JSString
toJSStr s = unsafeCoerce# (I# (unsafeCoerce# (toJSStr# (unsafeCoerce# s))))

jsShowD :: Double -> String
jsShowD d = unsafeCoerce# (fromJSStr# (jsShowD# d))

jsShowF :: Float -> String
jsShowF f = unsafeCoerce# (fromJSStr# (jsShowF# f))

jsShowW :: Word -> String
jsShowW w = unsafeCoerce# (fromJSStr# (jsShowW# w))

jsShowI :: Int -> String
jsShowI i = unsafeCoerce# (fromJSStr# (jsShowI# i))
