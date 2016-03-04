{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, BangPatterns, CPP #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif
-- | High level JavaScript foreign interface.
module Haste.Prim.Foreign (
    module Haste.Prim.Any,
    FFI, JSFunc, JSException (..),
    ffi, constant, export
#if __GLASGOW_HASKELL__ >= 710
    , safe_ffi, StaticPtr
#endif
  ) where
import Haste.Prim
import Haste.Prim.Any
#if __GLASGOW_HASKELL__ >= 710
import GHC.StaticPtr (StaticPtr, deRefStaticPtr)
#endif
import Unsafe.Coerce
import Control.Exception
import Data.Typeable

-- | A JS function.
type JSFun = JSAny

#ifdef __HASTE__
foreign import ccall __strict :: JSAny -> JSAny
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
foreign import ccall __createJSFunc :: Int -> JSAny -> IO JSAny
#else
__strict :: JSAny -> JSAny
__strict x = x
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
__createJSFunc :: Int -> JSAny -> IO JSAny
__createJSFunc _ = return undefined
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
  __ffi f !as !a = __ffi f (a' : as)
    where !a' = toAny a

{-# INLINE [0] ffiio #-}
-- | Apply the result of an FFI call.
ffiio :: FromAny a => JSFun -> [JSAny] -> IO a
ffiio !f !as = __apply f (toPtr as) >>= fromAny

{-# INLINE ffi #-}
-- | Creates a Haskell function from the given string of JavaScript code. If
--   this code is not well typed or is otherwise incorrect, your program may
--   crash or misbehave in mystifying ways. Haste makes a best-effort try to
--   save you from poorly typed JS here, but there are no guarantees.
--
--   For instance, the following WILL cause crazy behavior due to wrong types:
--
--   > ffi "(function(x) {return x+1;})" :: Int -> Int -> IO Int
--
--   In other words, this function is as unsafe as the JS it calls on. You
--   have been warned.
--
--   The imported JS is evaluated lazily, unless (a) it is a function object
--   in which case evaluation order does not affect the semantics of the
--   imported code, or if (b) the imported code is explicitly marked as strict:
--
--   > someFunction = ffi "__strict(someJSFunction)"
--
--   Literals which depends on some third party initialization, the existence
--   of a DOM tree or some other condition which is not fulfilled at load time
--   should *not* be marked strict.
ffi :: FFI a => JSString -> a
ffi s = __ffi f []
  where
    {-# NOINLINE f #-}
    f = __eval s

#if __GLASGOW_HASKELL__ >= 710
safe_ffi :: FFI a => StaticPtr JSString -> a
safe_ffi = ffi . deRefStaticPtr
#endif

-- | Create a Haskell value from a constant JS expression.
constant :: FromAny a => JSString -> a
constant = veryUnsafePerformIO . fromAny . __eval

-- Don't build intermediate list for functions of <= 5 arguments.
{-# RULES
"app0" [1] forall f. ffiio f [] = __app0 f >>= fromAny
"app1" [1] forall f a. ffiio f [a] = __app1 f a >>= fromAny
"app2" [1] forall f a b. ffiio f [b,a] = __app2 f a b >>= fromAny
"app3" [1] forall f a b c. ffiio f [c,b,a] = __app3 f a b c >>= fromAny
"app4" [1] forall f a b c d. ffiio f [d,c,b,a] = __app4 f a b c d >>= fromAny
"app5" [1] forall f a b c d e. ffiio f [e,d,c,b,a] =
                                 __app5 f a b c d e >>= fromAny
  #-}

-- | Export a symbol. That symbol may then be accessed from JavaScript through
--   @Haste.name()@ as a normal function. Remember, however, that if you are
--   using --with-js to include your JS, in conjunction with
--   --opt-minify or any option that implies it, you will instead need
--   to access your exports through @Haste[\'name\']()@, or Closure will mangle
--   your function names.
{-# INLINE export #-}
export :: ToAny a => JSString -> a -> IO ()
export = ffi "(function(s,f){Haste[s] = f;})"

type family JS a where
  JS (a -> b) = JSAny -> JS b
  JS (IO a)   = IO JSAny
  JS a        = JSAny

class JSFunc a where
  mkJSFunc :: a -> JS a
  arity    :: a -> Int

#if __GLASGOW_HASKELL__ < 710
instance (ToAny a, JS a ~ JSAny) => JSFunc a where
#else
instance {-# OVERLAPPABLE #-} (ToAny a, JS a ~ JSAny) => JSFunc a where
#endif
  mkJSFunc = toAny
  arity _  = 0

{-# INLINE strictly #-}
strictly :: a -> a
strictly x = unsafeCoerce (__strict (unsafeCoerce x))

{-# RULES
  "strictly" forall x. __strict x
    = unsafeCoerce x
  #-}

instance ToAny a => JSFunc (IO a) where
  mkJSFunc = fmap toAny
  arity _  = 1

instance (FromAny a, JSFunc b) => JSFunc (a -> b) where
  mkJSFunc f = mkJSFunc . f . strictly . veryUnsafePerformIO . fromAny
  arity f    = 1 + arity (f undefined)

instance (FromAny a, JSFunc b) => ToAny (a -> b) where
  toAny f =
    strictly . veryUnsafePerformIO . __createJSFunc (arity f) . toAny . toOpaque $ mkJSFunc f

instance ToAny a => ToAny (IO a) where
  toAny = strictly . veryUnsafePerformIO . __createJSFunc 0 . toAny . toOpaque . mkJSFunc

#if __GLASGOW_HASKELL__ < 710
instance FFI a => FromAny a where
#else
instance {-# OVERLAPPABLE #-} FFI a => FromAny a where
#endif
  fromAny f = return $ __ffi f []

-- | An exception raised from foreign JavaScript code.
data JSException = JSException JSString
  deriving (Show, Typeable)

instance Exception JSException where
#if __GLASGOW_HASKELL__ >= 710
  displayException (JSException e) = "JavaScript exception: " ++ fromJSStr e
#endif
