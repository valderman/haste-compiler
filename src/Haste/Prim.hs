{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}
module Haste.Prim (JSString, toJSStr, fromJSStr) where
import Foreign.Ptr

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
