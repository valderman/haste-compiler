-- For the FFI
{-# LANGUAGE ForeignFunctionInterface, PatternGuards, CPP, BangPatterns #-}

-- For generic default instances
{-# LANGUAGE TypeOperators, ScopedTypeVariables, FlexibleInstances,
             FlexibleContexts, OverloadedStrings, DefaultSignatures #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

-- For less annoying instances
{-# LANGUAGE TupleSections #-}

-- | Converting to/from JS-native data.
module Haste.Prim.Any (
    ToAny (..), FromAny (..), Generic, JSAny (..),
    Opaque, toOpaque, fromOpaque,
    nullValue, toObject, has, get, index
  ) where
import GHC.Generics
import Control.Exception
import Haste.Prim
import Haste.Prim.JSType
import Data.Int
import Data.Word
import Unsafe.Coerce
import System.IO.Unsafe -- for toObject
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

#ifdef __HASTE__
foreign import ccall __lst2arr :: Ptr [a] -> JSAny
foreign import ccall __arr2lst :: Int -> JSAny -> Ptr [a]
foreign import ccall "String" jsString :: JSAny -> JSString
foreign import ccall "Number" jsNumber :: JSAny -> Double
foreign import ccall __jsNull :: IO JSAny
foreign import ccall __new :: IO JSAny
foreign import ccall __set :: JSAny -> JSString -> JSAny -> IO ()
foreign import ccall __get :: JSAny -> JSString -> IO JSAny
foreign import ccall __has :: JSAny -> JSString -> IO Bool
#else
__new :: IO JSAny
__new = return undefined
__get :: JSAny -> JSString -> IO JSAny
__get _ _ = return undefined
__set :: JSAny -> JSString -> JSAny -> IO ()
__set _ _ _ = return ()
__has :: JSAny -> JSString -> IO Bool
__has _ _ = return False
__lst2arr :: Ptr [a] -> JSAny
__lst2arr _ = undefined
__arr2lst :: Int -> JSAny -> Ptr [a]
__arr2lst _ _ = undefined
jsString :: JSAny -> JSString
jsString _ = undefined
jsNumber :: JSAny -> Double
jsNumber _ = undefined
__jsNull :: IO JSAny
__jsNull = undefined
#endif

{-# NOINLINE jsNull #-}
jsNull :: JSAny
jsNull = unsafePerformIO __jsNull

{-
  For theoretical purposes, imagine the following here:
  foreign import ccall __intToAny :: Int -> JSAny
  ...
  In practice, however, we use unsafeCoerce for that to avoid the roundtrip.
-}

-- | The JS value null.
nullValue :: JSAny
nullValue = jsNull

-- | Build a new JS object from a list of key:value pairs.
toObject :: [(JSString, JSAny)] -> JSAny
toObject ps = veryUnsafePerformIO $ do
  o <- __new
  mapM_ (uncurry $ __set o) ps
  return o

-- | Read a member from a JS object. Throws an error if the member can not be
--   marshalled into a value of type @a@.
{-# INLINE get #-}
get :: FromAny a => JSAny -> JSString -> IO a
get o k = __get o k >>= fromAny

{-# INLINE index #-}
-- | Read an element from a JS array. Throws an error if the member can not be
--   marshalled into a value of type @a@.
index :: FromAny a => JSAny -> Int -> IO a
index o k = __get o (unsafeCoerce k) >>= fromAny

-- | Check if a JS object has a particular member.
{-# INLINE has #-}
has :: JSAny -> JSString -> IO Bool
has = __has

-- | Any type that can be converted into a JavaScript value.
class ToAny a where
  -- | Build a JS object from a Haskell value.
  --   The default instance creates an object from any type that derives
  --   'Generic' according to the following rules:
  --   * Records turn into plain JS objects, with record names as field names.
  --   * Non-record product types turn into objects containing a @$data@ field
  --     which contains all of the constructor's unnamed fields.
  --   * Values of enum types turn into strings matching their constructors.
  --   * Non-enum types with more than one constructor gain an extra field,
  --     @$tag@, which contains the name of the constructor used to create the
  --     object.
  toAny :: a -> JSAny
  default toAny :: (GToAny (Rep a), Generic a) => a -> JSAny
  toAny x =
    case gToAny False g of
      Tree x' -> toObject x'
      One  x' -> if isEnum g then x' else toAny [x']
      List x' -> toAny x'
    where g = from x

  listToAny :: [a] -> JSAny
  listToAny = __lst2arr . toPtr . map toAny

-- | Any type that can be converted from a JavaScript value.
class FromAny a where
  -- | Convert a value from JS with a reasonable conversion if an exact match
  --   is not possible. Examples of reasonable conversions would be truncating
  --   floating point numbers to integers, or turning signed integers into
  --   unsigned.
  fromAny :: JSAny -> IO a
  default fromAny :: (GToAny (Rep a), GFromAny (Rep a), Generic a)
                  => JSAny -> IO a
  fromAny x = to <$> gFromAny False x

  listFromAny :: JSAny -> IO [a]
  listFromAny = mapM fromAny . fromPtr . __arr2lst 0

-- | The Opaque type is inhabited by values that can be passed to JavaScript
--   using their raw Haskell representation. Opaque values are completely
--   useless to JS code, and should not be inspected. This is useful for,
--   for instance, storing data in some JS-native data structure for later
--   retrieval.
newtype Opaque a = Opaque {fromOpaque :: a}

toOpaque :: a -> Opaque a
toOpaque = Opaque



-- ToAny instances
instance ToAny JSAny where toAny = unsafeCoerce
instance ToAny (Ptr a) where toAny = unsafeCoerce
instance ToAny JSString where toAny = unsafeCoerce
instance ToAny Int where toAny = unsafeCoerce
instance ToAny Int8 where toAny = unsafeCoerce
instance ToAny Int16 where toAny = unsafeCoerce
instance ToAny Int32 where toAny = unsafeCoerce
instance ToAny Word where toAny = unsafeCoerce
instance ToAny Word8 where toAny = unsafeCoerce
instance ToAny Word16 where toAny = unsafeCoerce
instance ToAny Word32 where toAny = unsafeCoerce
instance ToAny Float where toAny = unsafeCoerce
instance ToAny Double where toAny = unsafeCoerce
instance ToAny Char where
  toAny = unsafeCoerce
  listToAny = toAny . toJSStr
instance ToAny () where
  toAny _ = jsNull
instance ToAny (Opaque a) where
  toAny (Opaque x) = unsafeCoerce $ toPtr x
instance ToAny Bool where
  toAny = unsafeCoerce

-- | Lists are marshalled into arrays, with the exception of 'String'.
instance ToAny a => ToAny [a] where
  toAny = listToAny

-- | Maybe is simply a nullable type. Nothing is equivalent to null, and any
--   non-null value is equivalent to x in Just x.
instance ToAny a => ToAny (Maybe a) where
  toAny Nothing  = jsNull
  toAny (Just x) = toAny x

-- | Tuples are marshalled into arrays.
instance (ToAny a, ToAny b) => ToAny (a, b) where
  toAny (a, b) = toAny [toAny a, toAny b]

instance (ToAny a, ToAny b, ToAny c) => ToAny (a, b, c) where
  toAny (a, b, c) = toAny [toAny a, toAny b, toAny c]

instance (ToAny a, ToAny b, ToAny c, ToAny d) =>
          ToAny (a, b, c, d) where
  toAny (a, b, c, d) = toAny [toAny a, toAny b, toAny c, toAny d]

instance (ToAny a, ToAny b, ToAny c, ToAny d, ToAny e) =>
          ToAny (a, b, c, d, e) where
  toAny (a, b, c, d, e) = toAny [toAny a,toAny b,toAny c,toAny d,toAny e]

instance (ToAny a, ToAny b, ToAny c, ToAny d, ToAny e,
          ToAny f) => ToAny (a, b, c, d, e, f) where
  toAny (a, b, c, d, e, f) =
    toAny [toAny a, toAny b, toAny c, toAny d, toAny e, toAny f]

instance (ToAny a, ToAny b, ToAny c, ToAny d, ToAny e,
          ToAny f, ToAny g) => ToAny (a, b, c, d, e, f, g) where
  toAny (a, b, c, d, e, f, g) =
    toAny [toAny a,toAny b,toAny c,toAny d,toAny e,toAny f,toAny g]



instance FromAny JSAny where
  fromAny x = return (unsafeCoerce x)
instance FromAny (Ptr a) where
  fromAny x = return (unsafeCoerce x)
instance FromAny JSString where
  fromAny x = return (jsString x)
instance FromAny Int where
  fromAny x = return (convert (jsNumber x))
instance FromAny Int8 where
  fromAny x = return (convert (jsNumber x))
instance FromAny Int16 where
  fromAny x = return (convert (jsNumber x))
instance FromAny Int32 where
  fromAny x = return (convert (jsNumber x))
instance FromAny Word where
  fromAny x = return (convert (jsNumber x))
instance FromAny Word8 where
  fromAny x = return (convert (jsNumber x))
instance FromAny Word16 where
  fromAny x = return (convert (jsNumber x))
instance FromAny Word32 where
  fromAny x = return (convert (jsNumber x))

-- unsafeCoerce for Float and Double saves a lot on performance, and only
-- differs in semantics if the value in question is a) not a number, and
-- b) passed verbatim back into JS land
instance FromAny Float where
  fromAny x = unsafeCoerce x
instance FromAny Double where
  fromAny x = unsafeCoerce x

instance FromAny Char where
  fromAny x = return (unsafeCoerce (jsNumber x))
  listFromAny x = fromJSStr <$> fromAny x
instance FromAny () where
  fromAny _ = return ()
instance FromAny (Opaque a) where
  fromAny x = Opaque . fromPtr <$> fromAny x
instance FromAny Bool where
  fromAny = return . unsafeCoerce

instance FromAny a => FromAny [a] where
  fromAny = listFromAny

instance FromAny a => FromAny (Maybe a) where
  fromAny x | x == jsNull = return Nothing
            | otherwise   = Just <$> fromAny x

instance (FromAny a, FromAny b) => FromAny (a, b) where
  fromAny x = do
    [a,b] <- fromAny x
    (,) <$> fromAny a <*> fromAny b

instance (FromAny a, FromAny b, FromAny c) => FromAny (a, b, c) where
  fromAny x = do
    [a,b,c] <- fromAny x
    (,,) <$> fromAny a <*> fromAny b <*> fromAny c

instance (FromAny a, FromAny b, FromAny c, FromAny d) =>
          FromAny (a, b, c, d) where
  fromAny x = do
    [a,b,c,d] <- fromAny x
    (,,,) <$> fromAny a <*> fromAny b <*> fromAny c <*> fromAny d

instance (FromAny a, FromAny b, FromAny c, FromAny d, FromAny e) =>
          FromAny (a, b, c, d, e) where
  fromAny x = do
    [a,b,c,d,e] <- fromAny x
    (,,,,) <$> fromAny a <*> fromAny b <*> fromAny c
           <*> fromAny d <*> fromAny e

instance (FromAny a, FromAny b, FromAny c, FromAny d, FromAny e, FromAny f) =>
          FromAny (a, b, c, d, e, f) where
  fromAny x = do
    [a,b,c,d,e,f] <- fromAny x
    (,,,,,) <$> fromAny a <*> fromAny b <*> fromAny c
            <*> fromAny d <*> fromAny e <*> fromAny f

instance (FromAny a, FromAny b, FromAny c, FromAny d,
          FromAny e, FromAny f, FromAny g) =>
          FromAny (a, b, c, d, e, f, g) where
  fromAny x = do
    [a,b,c,d,e,f,g] <- fromAny x
    (,,,,,,) <$> fromAny a <*> fromAny b <*> fromAny c <*> fromAny d
             <*> fromAny e <*> fromAny f <*> fromAny g

data Value = One !JSAny | List ![JSAny] | Tree ![(JSString, JSAny)]

-- GToAny instances
class GToAny f where
  gToAny :: Bool -> f a -> Value
  isEnum :: f a -> Bool

instance GToAny U1 where
  gToAny _ U1 = error "U1: unpossible!"
  isEnum _    = True

instance ToAny a => GToAny (K1 i a) where
  gToAny _ (K1 x) = One (toAny x)
  isEnum _ = False

instance (Selector c, GToAny a) => GToAny (M1 S c a) where
  gToAny mcs (M1 x) = do
    case name of
      "" -> One  value
      _  -> Tree [(name, value)]
    where name  = toJSStr (selName (undefined :: M1 S c a ()))
          value =
            case gToAny mcs x of
              Tree x' -> toObject x'
              One  x' -> toAny x'
              List x' -> toAny x'
  isEnum _ = isEnum (undefined :: a ())

instance Constructor c => GToAny (M1 C c U1) where
  gToAny _ _ = One (toAny $ conName (undefined :: M1 C c U1 ()))
  isEnum _ = True

#if __GLASGOW_HASKELL__ < 710
instance (Constructor c, GToAny a) => GToAny (M1 C c a) where
#else
instance {-# OVERLAPPABLE #-} (Constructor c, GToAny a) =>
                               GToAny (M1 C c a) where
#endif
  gToAny many_constrs (M1 x)
    | many_constrs =
      case args of
        Tree args' -> Tree (("$tag", toAny tag) : args')
        One  arg   -> Tree [("$tag", toAny tag), ("$data", arg)]
        List args' -> Tree [("$tag", toAny tag), ("$data", toAny args')]
    | otherwise =
      args
    where
      tag  = conName (undefined :: M1 C c a ())
      args = gToAny many_constrs x
  isEnum _ = isEnum (undefined :: a ())

instance GToAny a => GToAny (M1 D c a) where
  gToAny cs (M1 x) = gToAny cs x
  isEnum _ = isEnum (undefined :: a ())

instance (GToAny a, GToAny b) => GToAny (a :*: b) where
  gToAny cs (a :*: b) =
    case (gToAny cs a, gToAny cs b) of
      (One l,   One r)   -> List [l, r]
      (One x,   List xs) -> List (x:xs)
      (List xs, One x)   -> List (xs ++ [x])
      (List l,  List r)  -> List (l ++ r)
      (Tree l,  Tree r)  -> Tree (l ++ r)
      (_,       _)       -> error "Tree :*: non-tree!"
  isEnum _ = False

instance (GToAny a, GToAny b) => GToAny (a :+: b) where
  gToAny _ (L1 x) = gToAny True x
  gToAny _ (R1 x) = gToAny True x
  isEnum _ = isEnum (undefined :: a ()) && isEnum (undefined :: b ())





-- GFromAny instances
class GFromAny f where
  gFromAny   :: Bool -> JSAny -> IO (f a)
  isRecord   :: f a -> Bool
  gFromList  :: Int -> JSAny -> IO (f a, Int)
  gFromList = error "gFromList called on non-product!"

instance GFromAny U1 where
  gFromAny _ _ = return U1
  isRecord _ = False

instance (ToAny a, FromAny a) => GFromAny (K1 i a) where
  gFromAny _ x = do
    x' <- fromAny x
    return $ K1 x'
  isRecord _ = False
  gFromList ix x = do
    x' <- x `index` ix
    return (K1 x', ix+1)

instance (Selector c, GFromAny a) => GFromAny (M1 S c a) where
  gFromAny mcs x = do
      exists <- x `has` prop
      if exists
        then M1 <$> (get x prop >>= gFromAny mcs)
        else error $ "No such member: '" ++ sn ++ "'"
    where
      sn   = selName (undefined :: M1 S c a ())
      prop = toJSStr sn
  isRecord _ = not (null $ selName (undefined :: M1 S c a ()))
  gFromList ix x = do
    (x', ix') <- gFromList ix x
    return (M1 x', ix')

instance Constructor c => GFromAny (M1 C c U1) where
  gFromAny _ x = do
      n <- fromAny x
      if (n == cn)
        then return $ M1 U1
        else error $  "Couldn't fromAny constructor: expected " ++ cn
                   ++ " but got " ++ n
    where
      cn = conName (undefined :: M1 C c U1 ())
  isRecord _ = False

#if __GLASGOW_HASKELL__ < 710
instance (Constructor c, GFromAny a) => GFromAny (M1 C c a) where
#else
instance {-# OVERLAPPABLE #-} (Constructor c, GFromAny a) =>
                               GFromAny (M1 C c a) where
#endif
  gFromAny many_constrs x
    | many_constrs = do
        t <- x `get` "$tag"
        if t == tag
          then M1 <$> (x `get` "$data" >>= gFromAny many_constrs)
          else error $  "Couldn't fromAny constructor: expected " ++ tag
                     ++ " but got " ++ t
    | isRecord (undefined :: a ()) = do
        M1 <$> gFromAny many_constrs x
    | otherwise = do
        M1 . fst <$> gFromList 0 x
    where
      tag = conName (undefined :: M1 C c a ())
  isRecord _ = isRecord (undefined :: a ())

instance GFromAny a => GFromAny (M1 D c a) where
  gFromAny cs x = M1 <$> gFromAny cs x
  isRecord _ = isRecord (undefined :: a ())
  gFromList ix x = do
    (x', ix') <- gFromList ix x
    return (M1 x', ix')

instance (GFromAny a, GFromAny b) => GFromAny (a :*: b) where
  gFromAny cs x = do
    a <- gFromAny cs x
    b <- gFromAny cs x
    return (a :*: b)
  isRecord _ = isRecord (undefined :: a ())
  gFromList ix x = do
    (a, ix') <- gFromList ix x
    (b, ix'') <- gFromList ix' x
    return (a :*: b, ix'')

instance (GFromAny a, GFromAny b) => GFromAny (a :+: b) where
  gFromAny _ x = do
    catch (L1 <$> gFromAny True x)
          (withSomeException $ R1 <$> gFromAny True x)
  isRecord _ = isRecord (undefined :: a ()) || isRecord (undefined :: b ())

withSomeException :: IO a -> SomeException -> IO a
withSomeException m _ = m

foreign import ccall "jsLog" dbg :: JSAny -> IO ()
