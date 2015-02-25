{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, TypeSynonymInstances,
             FlexibleInstances, TypeFamilies, OverlappingInstances, CPP,
             OverloadedStrings, UndecidableInstances #-}
-- | Create functions on the fly from JS strings.
--   Slower but more flexible alternative to the standard FFI.
module Haste.Foreign (
    FFI, Pack (..), Unpack (..), Marshal,
    Unpacked, Opaque,
    ffi, export, toOpaque, fromOpaque
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

class Pack a where
  pack :: Unpacked -> a
  pack = unsafePack

class Unpack a where
  unpack :: a -> Unpacked
  unpack = unsafeUnpack

-- | Class for marshallable types. Pack takes an opaque JS value and turns it
--   into the type's proper Haste representation, and unpack is its inverse.
--   The default instances make an effort to prevent wrongly typed values
--   through, but you could probably break them with enough creativity.
class (Pack a, Unpack a) => Marshal a
instance (Pack a, Unpack a) => Marshal a

instance Pack Float
instance Pack Double
instance Pack JSAny
instance Pack JSString where
  pack = jsString . unsafePack
instance Pack Char where
  pack x = convert (unsafePack x :: Double)
instance Pack Int where
  pack x = convert (unsafePack x :: Double)
instance Pack Int8 where
  pack x = convert (unsafePack x :: Double)
instance Pack Int16 where
  pack x = convert (unsafePack x :: Double)
instance Pack Int32 where
  pack x = convert (unsafePack x :: Double)
instance Pack Word where
  pack x = convert (unsafePack x :: Double)
instance Pack Word8 where
  pack x = convert (unsafePack x :: Double)
instance Pack Word16 where
  pack x = convert (unsafePack x :: Double)
instance Pack Word32 where
  pack x = convert (unsafePack x :: Double)
instance Pack () where
  pack _   = ()
instance Pack String where
  pack = fromJSStr . pack
instance Pack Unpacked where
  pack = id
instance Pack (Opaque a) where
  pack = Opaque
instance Pack Bool where
  pack x = if pack x > (0 :: Double) then True else False

-- | Lists are marshalled into arrays.
instance Pack a => Pack [a] where
  pack arr = map pack . fromOpaque $ arr2lst arr 0

-- | Maybe is simply a nullable type. Nothing is equivalent to null, and any
--   non-null value is equivalent to x in Just x.
instance Pack a => Pack (Maybe a) where
  pack x = if isNull x then Nothing else Just (pack x)

-- | Tuples are marshalled into arrays.
instance (Pack a, Pack b) => Pack (a, b) where
  pack x = case pack x of [a, b] -> (pack a, pack b)

instance (Pack a, Pack b, Pack c) => Pack (a, b, c) where
  pack x = case pack x of [a, b, c] -> (pack a, pack b, pack c)

instance (Pack a, Pack b, Pack c, Pack d) =>
         Pack (a, b, c, d) where
  pack x = case pack x of [a, b, c, d] -> (pack a, pack b, pack c, pack d)

instance (Pack a, Pack b, Pack c, Pack d, Pack e) =>
         Pack (a, b, c, d, e) where
  pack x = case pack x of [a,b,c,d,e] -> (pack a, pack b, pack c, pack d, pack e)

instance (Pack a, Pack b, Pack c, Pack d, Pack e,
          Pack f) => Pack (a, b, c, d, e, f) where
  pack x = case pack x of
    [a, b, c, d, e, f] -> (pack a, pack b, pack c, pack d, pack e, pack f)

instance (Pack a, Pack b, Pack c, Pack d, Pack e,
          Pack f, Pack g) => Pack (a, b, c, d, e, f, g) where
  pack x = case pack x of
    [a, b, c, d, e, f, g] -> (pack a,pack b,pack c,pack d,pack e,pack f,pack g)

instance (Pack a, Pack b, Pack c, Pack d, Pack e,
          Pack f, Pack g, Pack h) =>
         Pack (a, b, c, d, e, f, g, h) where
  pack x = case pack x of
    [a, b, c, d, e, f, g, h] -> (pack a, pack b, pack c, pack d, pack e,
                                 pack f, pack g, pack h)

instance (Pack a, Pack b, Pack c, Pack d, Pack e,
          Pack f, Pack g, Pack h, Pack i) =>
         Pack (a, b, c, d, e, f, g, h, i) where
  pack x = case pack x of
    [a, b, c, d, e, f, g, h, i] -> (pack a, pack b, pack c, pack d, pack e,
                                    pack f, pack g, pack h, pack i)

instance (Pack a, Pack b, Pack c, Pack d, Pack e,
          Pack f, Pack g, Pack h, Pack i, Pack j) =>
         Pack (a, b, c, d, e, f, g, h, i, j) where
  pack x = case pack x of
    [a, b, c, d, e, f, g, h, i, j] -> (pack a, pack b, pack c, pack d, pack e,
                                       pack f, pack g, pack h, pack i, pack j)

instance Unpack Float
instance Unpack Double
instance Unpack JSAny
instance Unpack JSString
instance Unpack Char
instance Unpack Int
instance Unpack Int8
instance Unpack Int16
instance Unpack Int32
instance Unpack Word
instance Unpack Word8
instance Unpack Word16
instance Unpack Word32
instance Unpack () where
  unpack _ = unpack (0 :: Double)
instance Unpack String where
  unpack = unpack . toJSStr
instance Unpack Unpacked where
  unpack = id
instance Unpack (Opaque a) where
  unpack (Opaque x) = x
instance Unpack Bool where
  unpack True  = jsTrue
  unpack False = jsFalse

-- | Lists are marshalled into arrays.
instance Unpack a => Unpack [a] where
  unpack = lst2arr . toOpaque . map unpack

-- | Maybe is simply a nullable type. Nothing is equivalent to null, and any
--   non-null value is equivalent to x in Just x.
instance Unpack a => Unpack (Maybe a) where
  unpack Nothing  = jsNull
  unpack (Just x) = unpack x

-- | Tuples are marshalled into arrays.
instance (Unpack a, Unpack b) => Unpack (a, b) where
  unpack (a, b) = unpack [unpack a, unpack b]

instance (Unpack a, Unpack b, Unpack c) => Unpack (a, b, c) where
  unpack (a, b, c) = unpack [unpack a, unpack b, unpack c]

instance (Unpack a, Unpack b, Unpack c, Unpack d) =>
         Unpack (a, b, c, d) where
  unpack (a, b, c, d) = unpack [unpack a, unpack b, unpack c, unpack d]

instance (Unpack a, Unpack b, Unpack c, Unpack d, Unpack e) =>
         Unpack (a, b, c, d, e) where
  unpack (a, b, c, d, e) = unpack [unpack a,unpack b,unpack c,unpack d,unpack e]

instance (Unpack a, Unpack b, Unpack c, Unpack d, Unpack e,
          Unpack f) => Unpack (a, b, c, d, e, f) where
  unpack (a, b, c, d, e, f) =
    unpack [unpack a, unpack b, unpack c, unpack d, unpack e, unpack f]

instance (Unpack a, Unpack b, Unpack c, Unpack d, Unpack e,
          Unpack f, Unpack g) => Unpack (a, b, c, d, e, f, g) where
  unpack (a, b, c, d, e, f, g) =
    unpack [unpack a,unpack b,unpack c,unpack d,unpack e,unpack f,unpack g]

instance (Unpack a, Unpack b, Unpack c, Unpack d, Unpack e,
          Unpack f, Unpack g, Unpack h) =>
         Unpack (a, b, c, d, e, f, g, h) where
  unpack (a, b, c, d, e, f, g, h) =
    unpack [unpack a, unpack b, unpack c, unpack d, unpack e,
            unpack f, unpack g, unpack h]

instance (Unpack a, Unpack b, Unpack c, Unpack d, Unpack e,
          Unpack f, Unpack g, Unpack h, Unpack i) =>
         Unpack (a, b, c, d, e, f, g, h, i) where
  unpack (a, b, c, d, e, f, g, h, i) =
    unpack [unpack a, unpack b, unpack c, unpack d, unpack e,
            unpack f, unpack g, unpack h, unpack i]

instance (Unpack a, Unpack b, Unpack c, Unpack d, Unpack e,
          Unpack f, Unpack g, Unpack h, Unpack i, Unpack j) =>
         Unpack (a, b, c, d, e, f, g, h, i, j) where
  unpack (a, b, c, d, e, f, g, h, i, j) =
    unpack [unpack a, unpack b, unpack c, unpack d, unpack e,
            unpack f, unpack g, unpack h, unpack i, unpack j]

{-# RULES "unpack array/Unpacked" forall x. unpack x = lst2arr (toOpaque x) #-}
{-# RULES "pack array/Unpacked" forall x. pack x = fromOpaque (arr2lst x 0) #-}

lst2arr :: Opaque [Unpacked] -> Unpacked
lst2arr = unsafePerformIO . ffi "lst2arr"

arr2lst :: Unpacked -> Int -> Opaque [Unpacked]
arr2lst arr ix = unsafePerformIO $ ffi "arr2lst" arr ix

jsNull, jsTrue, jsFalse :: Unpacked
jsTrue = unsafePerformIO $ ffi "true"
jsFalse = unsafePerformIO $ ffi "false"
jsNull = unsafePerformIO $ ffi "null"

isNull :: Unpacked -> Bool
isNull = unsafePerformIO . ffi "(function(x) {return x === null;})"

class FFI a where
  type T a
  unpackify :: T a -> a

instance Pack a => FFI (IO a) where
  type T (IO a) = IO Unpacked
  unpackify = fmap pack

instance (Unpack a, FFI b) => FFI (a -> b) where
  type T (a -> b) = Unpacked -> T b
  unpackify f x = unpackify (f $! unpack x)

class IOFun a where
  type X a
  packify :: a -> X a

instance Unpack a => IOFun (IO a) where
  type X (IO a) = Unpacked
  packify m = unsafePerformIO $ do
    x <- m
    return $! unpack x

instance (Pack a, IOFun b) => IOFun (a -> b) where
  type X (a -> b) = Unpacked -> X b
  packify f = \x -> packify (f $! pack x)

instance Unpack a => Unpack (IO a) where
  unpack = unsafePerformIO . unpackAct . toOpaque . fmap unpack
    where
      {-# NOINLINE unpackAct #-}
      unpackAct :: Opaque (IO Unpacked) -> IO Unpacked
      unpackAct =
        ffi (toJSStr $ "(function(m){" ++
             "    return (function() {" ++
             "        return (function(){return E(B(A(m,[0])));});" ++
             "      });" ++
             "})")

instance (IOFun (a -> b)) => Unpack (a -> b) where
  unpack = unpackFun

unpackFun :: IOFun a => a -> Unpacked
unpackFun =
    unsafePerformIO . go . toOpaque . packify
  where
    {-# NOINLINE go #-}
    go :: Opaque a -> IO Unpacked
    go = ffi (toJSStr $ "(function(f) {" ++
              "  return (function() {" ++
              "      return (function(){" ++
              "        var args=Array.prototype.slice.call(arguments,0);"++
              "        args.push(0);" ++
              "        return E(B(A(f, args)));" ++
              "    });" ++
              "  });" ++
              "})")


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
{-# NOINLINE export #-}
export :: Unpack a => JSString -> a -> IO ()
export = ffi "(function(s,f){Haste[s] = f;})"

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
