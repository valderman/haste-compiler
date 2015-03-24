{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, BangPatterns, CPP #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, OverlappingInstances, UndecidableInstances #-}
module Haste.Foreign (module Haste.Any, FFI, ffi, constant, export) where
import Haste.Prim
import Haste.Any
import GHC.Prim
import Unsafe.Coerce
import System.IO.Unsafe
import Data.String

-- | A JS function.
type JSFun = JSAny

-- | A JS array.
type JSArr = JSAny

#ifdef __HASTE__
foreign import ccall "eval" __eval :: JSString -> JSFun
foreign import ccall __apply :: JSFun -> Ptr [JSAny] -> IO JSAny
foreign import ccall __app0  :: JSFun -> IO JSAny
foreign import ccall __app1  :: JSFun -> JSAny -> IO JSAny
foreign import ccall __app2  :: JSFun -> JSAny -> JSAny -> IO JSAny
foreign import ccall __app3  :: JSFun -> JSAny -> JSAny -> JSAny -> IO JSAny
foreign import ccall __app4  :: JSFun
                             -> JSAny -> JSAny -> JSAny -> JSAny -> IO JSAny
foreign import ccall __app5  :: JSFun
                             -> JSAny -> JSAny -> JSAny -> JSAny -> JSAny
                             -> IO JSAny
foreign import ccall "A" hasteApply :: JSAny -> JSArr -> JSAny
#else
__eval :: JSString -> JSFun
__eval _ = undefined
__apply :: JSFun -> Ptr [JSAny] -> IO JSAny
__apply _ _ = return undefined
__app0  :: JSFun -> IO JSAny
__app0 _ = return undefined
__app1  :: JSFun -> JSAny -> IO JSAny
__app1 _ _ = return undefined
__app2  :: JSFun -> JSAny -> JSAny -> IO JSAny
__app2 _ _ _ = return undefined
__app3  :: JSFun -> JSAny -> JSAny -> JSAny -> IO JSAny
__app3 _ _ _ _ = return undefined
__app4  :: JSFun -> JSAny -> JSAny -> JSAny -> JSAny -> IO JSAny
__app4 _ _ _ _ _ = return undefined
__app5  :: JSFun -> JSAny -> JSAny -> JSAny -> JSAny -> JSAny -> IO JSAny
__app5 _ _ _ _ _ _ = return undefined
hasteApply :: JSAny -> JSArr -> JSAny
hasteApply _ _ = undefined
#endif

-- | Any type that can be imported from JavaScript. This means any type which
--   has an instance of 'FromAny', and any function where all argument types
--   has 'ToAny' instances and the return type is in the IO monad and has a
--   'FromAny' instance.
class FFI a where
  __ffi :: JSFun -> [JSAny] -> a

instance FromAny a => FFI (IO a) where
  {-# INLINE __ffi #-}
  __ffi = ffiio

instance (ToAny a, FFI b) => FFI (a -> b) where
  {-# INLINE __ffi #-}
  __ffi f !as a = __ffi f (a' : as)
    where !a' = toAny a

{-# NOINLINE [0] ffiio #-}
-- | Apply the result of an FFI call.
ffiio :: FromAny a => JSFun -> [JSAny] -> IO a
ffiio f !as = fromAny `fmap` __apply f (toPtr as)

{-# INLINE ffi #-}
-- | Creates a Haskell function from the given string of JavaScript code. If
--   this code is not well typed or is otherwise incorrect, your program may
--   crash or misbehave in mystifying ways. Haste makes a best-effort try to
--   save you from poorly typed JS here, but there are no guarantees.
--
--   For instance, the following WILL cause crazy behavior due to wrong types:
--   @ffi "(function(x) {return x+1;})" :: Int -> Int -> IO Int@
--
--   In other words, this function is as unsafe as the JS it calls on. You
--   have been warned.
ffi :: FFI a => JSString -> a
ffi s = __ffi f []
  where
    {-# NOINLINE f #-}
    f = __eval s

-- | Create a Haskell value from a constant JS expression.
constant :: FromAny a => JSString -> a
constant = fromAny . __eval

-- Don't build intermediate list for functions of <= 5 arguments.
{-# RULES
"app0" [1] forall f. ffiio f [] = fromAny `fmap` __app0 f
"app1" [1] forall f a. ffiio f [a] = fromAny `fmap` __app1 f a
"app2" [1] forall f a b. ffiio f [b,a] = fromAny `fmap` __app2 f a b
"app3" [1] forall f a b c. ffiio f [c,b,a] = fromAny `fmap` __app3 f a b c
"app4" [1] forall f a b c d. ffiio f [d,c,b,a] = fromAny `fmap` __app4 f a b c d
"app5" [1] forall f a b c d e. ffiio f [e,d,c,b,a] =
                                 fromAny `fmap` __app5 f a b c d e
  #-}

-- | Export a symbol. That symbol may then be accessed from JavaScript through
--   Haste.name() as a normal function. Remember, however, that if you are
--   using --with-js to include your JS, in conjunction with
--   --opt-minify or any option that implies it, you will instead need
--   to access your exports through Haste[\'name\'](), or Closure will mangle
--   your function names.
{-# NOINLINE export #-}
export :: ToAny a => JSString -> a -> IO ()
export = ffi "(function(s,f){Haste[s] = f;})"

type family JS a where
  JS (a -> b) = JSAny -> JS b
  JS (IO a)   = IO JSAny

class JSFunc a where
  mkJSFunc :: a -> JS a

instance ToAny a => JSFunc (IO a) where
  mkJSFunc = fmap toAny

instance (FromAny a, JSFunc b) => JSFunc (a -> b) where
  mkJSFunc f = \x -> mkJSFunc (f $! fromAny x)

instance JSFunc a => ToAny a where
  toAny = unsafePerformIO . createJSFunc . toAny . toOpaque . mkJSFunc

createJSFunc :: JSAny -> IO JSAny
createJSFunc =
  ffi "(function(f){return (function(){\
var as = Array.prototype.slice.call(arguments);\
for(var i in as) {as[i]=[0,as[i]];}\
as.push(0);\
return E(B(A(f,as)))[1];\
});})"
