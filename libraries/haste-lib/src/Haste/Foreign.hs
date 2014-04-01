{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, TypeSynonymInstances,
             FlexibleInstances, TypeFamilies, OverlappingInstances, CPP,
             OverloadedStrings #-}
-- | Create functions on the fly from JS strings.
--   Slower but more flexible alternative to the standard FFI.
module Haste.Foreign (
    FFI, Marshal (..), Unpacked, Opaque, ffi, export, toOpaque, fromOpaque
  ) where
import Haste.Prim
import Haste.JSType
import Data.Word
import Data.Int
import System.IO.Unsafe
import Unsafe.Coerce

#ifdef __HASTE__
foreign import ccall eval :: JSString -> IO (Ptr a)
foreign import ccall "String" jsString :: Double -> JSString
#else
eval :: JSString -> IO (Ptr a)
eval = error "Tried to use eval on server side!"
jsString :: Double -> JSString
jsString = error "Tried to use jsString on server side!"
#endif

-- | Opaque type representing a raw, unpacked JS value. The constructors have
--   no meaning, but are only there to make sure GHC doesn't optimize the low
--   level hackery in this module into oblivion.
data Unpacked = A | B

-- | The Opaque type is inhabited by values that can be passed to Javascript
--   using their raw Haskell representation. Opaque values are completely
--   useless to Javascript code, and should not be inspected. This is useful
--   for, for instance, storing data in some Javascript-native data structure
--   for later retrieval.
newtype Opaque a = Opaque Unpacked

toOpaque :: a -> Opaque a
toOpaque = unsafeCoerce

fromOpaque :: Opaque a -> a
fromOpaque = unsafeCoerce

data Dummy = Dummy Unpacked

-- | Class for marshallable types. Pack takes an opaque JS value and turns it
--   into the type's proper Haste representation, and unpack is its inverse.
--   The default instances make an effort to prevent wrongly typed values
--   through, but you could probably break them with enough creativity.
class Marshal a where
  pack :: Unpacked -> a
  pack = unsafePack

  unpack :: a -> Unpacked
  unpack = unsafeUnpack

instance Marshal Float
instance Marshal Double
instance Marshal JSAny
instance Marshal JSString where
  pack = jsString . unsafePack
instance Marshal Int where
  pack x = convert (unsafePack x :: Double)
instance Marshal Int8 where
  pack x = convert (unsafePack x :: Double)
instance Marshal Int16 where
  pack x = convert (unsafePack x :: Double)
instance Marshal Int32 where
  pack x = convert (unsafePack x :: Double)
instance Marshal Word where
  pack x = convert (unsafePack x :: Double)
instance Marshal Word8 where
  pack x = convert (unsafePack x :: Double)
instance Marshal Word16 where
  pack x = convert (unsafePack x :: Double)
instance Marshal Word32 where
  pack x = convert (unsafePack x :: Double)
instance Marshal () where
  pack _   = ()
  unpack _ = unpack (0 :: Double)
instance Marshal String where
  pack = fromJSStr . pack
  unpack = unpack . toJSStr
instance Marshal Unpacked where
  pack = id
  unpack = id
instance Marshal (Opaque a) where
  pack = Opaque
  unpack (Opaque x) = x
instance Marshal Bool where
  unpack True  = jsTrue
  unpack False = jsFalse
  pack x = if pack x > (0 :: Double) then True else False

-- | Tuples are marshalled into arrays.
instance (Marshal a, Marshal b) => Marshal (a, b) where
  unpack (a, b) = unpack [unpack a, unpack b]
  pack x = case pack x of [a, b] -> (pack a, pack b)

instance (Marshal a, Marshal b, Marshal c) => Marshal (a, b, c) where
  unpack (a, b, c) = unpack [unpack a, unpack b, unpack c]
  pack x = case pack x of [a, b, c] -> (pack a, pack b, pack c)

instance (Marshal a, Marshal b, Marshal c, Marshal d) =>
         Marshal (a, b, c, d) where
  unpack (a, b, c, d) = unpack [unpack a, unpack b, unpack c, unpack d]
  pack x = case pack x of [a, b, c, d] -> (pack a, pack b, pack c, pack d)

instance (Marshal a, Marshal b, Marshal c, Marshal d, Marshal e) =>
         Marshal (a, b, c, d, e) where
  unpack (a, b, c, d, e) = unpack [unpack a,unpack b,unpack c,unpack d,unpack e]
  pack x = case pack x of [a,b,c,d,e] -> (pack a, pack b, pack c, pack d, pack e)

instance (Marshal a, Marshal b, Marshal c, Marshal d, Marshal e,
          Marshal f) => Marshal (a, b, c, d, e, f) where
  unpack (a, b, c, d, e, f) =
    unpack [unpack a, unpack b, unpack c, unpack d, unpack e, unpack f]
  pack x = case pack x of
    [a, b, c, d, e, f] -> (pack a, pack b, pack c, pack d, pack e, pack f)

instance (Marshal a, Marshal b, Marshal c, Marshal d, Marshal e,
          Marshal f, Marshal g) => Marshal (a, b, c, d, e, f, g) where
  unpack (a, b, c, d, e, f, g) =
    unpack [unpack a,unpack b,unpack c,unpack d,unpack e,unpack f,unpack g]
  pack x = case pack x of
    [a, b, c, d, e, f, g] -> (pack a,pack b,pack c,pack d,pack e,pack f,pack g)

instance (Marshal a, Marshal b, Marshal c, Marshal d, Marshal e,
          Marshal f, Marshal g, Marshal h) =>
         Marshal (a, b, c, d, e, f, g, h) where
  unpack (a, b, c, d, e, f, g, h) =
    unpack [unpack a, unpack b, unpack c, unpack d, unpack e,
            unpack f, unpack g, unpack h]
  pack x = case pack x of
    [a, b, c, d, e, f, g, h] -> (pack a, pack b, pack c, pack d, pack e,
                                 pack f, pack g, pack h)

instance (Marshal a, Marshal b, Marshal c, Marshal d, Marshal e,
          Marshal f, Marshal g, Marshal h, Marshal i) =>
         Marshal (a, b, c, d, e, f, g, h, i) where
  unpack (a, b, c, d, e, f, g, h, i) =
    unpack [unpack a, unpack b, unpack c, unpack d, unpack e,
            unpack f, unpack g, unpack h, unpack i]
  pack x = case pack x of
    [a, b, c, d, e, f, g, h, i] -> (pack a, pack b, pack c, pack d, pack e,
                                    pack f, pack g, pack h, pack i)

instance (Marshal a, Marshal b, Marshal c, Marshal d, Marshal e,
          Marshal f, Marshal g, Marshal h, Marshal i, Marshal j) =>
         Marshal (a, b, c, d, e, f, g, h, i, j) where
  unpack (a, b, c, d, e, f, g, h, i, j) =
    unpack [unpack a, unpack b, unpack c, unpack d, unpack e,
            unpack f, unpack g, unpack h, unpack i, unpack j]
  pack x = case pack x of
    [a, b, c, d, e, f, g, h, i, j] -> (pack a, pack b, pack c, pack d, pack e,
                                       pack f, pack g, pack h, pack i, pack j)

-- | Lists are marshalled into arrays.
instance Marshal a => Marshal [a] where
  unpack = lst2arr . toOpaque . map unpack
  pack arr = map pack . fromOpaque $ arr2lst arr 0

{-# RULES "unpack array/Unpacked" forall x. unpack x = lst2arr (toOpaque x) #-}
{-# RULES "pack array/Unpacked" forall x. pack x = fromOpaque (arr2lst x 0) #-}

lst2arr :: Opaque [Unpacked] -> Unpacked
lst2arr = unsafePerformIO . ffi "lst2arr"

arr2lst :: Unpacked -> Int -> Opaque [Unpacked]
arr2lst arr ix = unsafePerformIO $ ffi "arr2lst" arr ix

-- | Maybe is simply a nullable type. Nothing is equivalent to null, and any
--   non-null value is equivalent to x in Just x.
instance Marshal a => Marshal (Maybe a) where
  unpack Nothing  = jsNull
  unpack (Just x) = unpack x
  pack x = if isNull x then Nothing else Just (pack x)

jsNull, jsTrue, jsFalse :: Unpacked
jsTrue = unsafePerformIO $ ffi "true"
jsFalse = unsafePerformIO $ ffi "false"
jsNull = unsafePerformIO $ ffi "null"

isNull :: Unpacked -> Bool
isNull = unsafePerformIO . ffi "(function(x) {return x === null;})"

class FFI a where
  type T a
  unpackify :: T a -> a
  -- | TODO: although the @export@ function, which is the only user-visible
  --         interface to @packify@, is type safe, the function itself is not.
  --         This should be fixed ASAP!
  packify :: a -> a

instance Marshal a => FFI (IO a) where
  type T (IO a) = IO Unpacked
  unpackify = fmap pack
  packify m = fmap (unsafeCoerce . unpack) m

instance (Marshal a, FFI b) => FFI (a -> b) where
  type T (a -> b) = Unpacked -> T b
  unpackify f x = unpackify (f $! unpack x)
  packify f x = packify (f $! pack (unsafeCoerce x))

-- | Creates a function based on the given string of Javascript code. If this
--   code is not well typed or is otherwise incorrect, your program may crash
--   or misbehave in mystifying ways. Haste makes a best-effort try to save you
--   from poorly typed JS here, but there are no guarantees.
--
--   For instance, the following WILL cause crazy behavior due to wrong types:
--   ffi "(function(x) {return x+1;})" :: Int -> Int -> IO Int
--
--   In other words, this function is completely unsafe - use with caution.
--
--   ALWAYS use type signatures for functions defined using this function, as
--   the argument marshalling is decided by the type signature.
ffi :: FFI a => JSString -> a
ffi = unpackify . unsafeEval

-- | Export a symbol. That symbol may then be accessed from Javascript through
--   Haste.name() as a normal function. Remember, however, that if you are
--   using --with-js to include your JS, in conjunction with
--   --opt-google-closure or any option that implies it, you will instead need
--   to access your exports through Haste[\'name\'](), or Closure will mangle
--   your function names.
export :: FFI a => JSString -> a -> IO ()
export name f =
    ffi (toJSStr $ "(function(s, f) {" ++
         "  Haste[s] = function() {" ++
         "      var args = Array.prototype.slice.call(arguments,0);" ++
         "      args.push(0);" ++
         "      return E(A(f, args));" ++
         "    };" ++
         "  return 0;" ++
         "})") name f'
  where
    f' :: Unpacked
    f' = unsafeCoerce $! packify f

unsafeUnpack :: a -> Unpacked
unsafeUnpack x =
  case unsafeCoerce x of
    Dummy x' -> x'

unsafePack :: Unpacked -> a
unsafePack = unsafeCoerce . Dummy

unsafeEval :: JSString -> a
unsafeEval s = unsafePerformIO $ do
  x <- eval s
  return $ fromPtr x
