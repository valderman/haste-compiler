-- For the FFI
{-# LANGUAGE ForeignFunctionInterface, PatternGuards, CPP, BangPatterns #-}

-- For generic default instances
{-# LANGUAGE TypeOperators, ScopedTypeVariables, FlexibleInstances,
             FlexibleContexts, OverloadedStrings, DefaultSignatures #-}

-- For less annoying instances
{-# LANGUAGE TupleSections #-}

-- | Converting to/from JS-native data.
module Haste.Any (
    ToAny (..), FromAny (..), Generic, JSAny (..),
    Opaque, toOpaque, fromOpaque,
    nullValue, mkObj
  ) where
import GHC.Generics
import Haste.Prim
import Haste.JSType
import Data.Int
import Data.Word
import Unsafe.Coerce
import Control.Applicative
import System.IO.Unsafe -- for mkObj

#ifdef __HASTE__
foreign import ccall __lst2arr :: Ptr [a] -> JSAny
foreign import ccall __arr2lst :: Int -> JSAny -> Ptr [a]
foreign import ccall "String" jsString :: JSAny -> JSString
foreign import ccall "Number" jsNumber :: JSAny -> Double
foreign import ccall "__jsNull" jsNull :: JSAny
foreign import ccall "__jsTrue" jsTrue :: JSAny
foreign import ccall "__jsFalse" jsFalse :: JSAny
foreign import ccall __new :: IO JSAny
foreign import ccall __set :: JSAny -> JSString -> JSAny -> IO ()
#else
__new :: IO JSAny
__new = return undefined
__set :: JSAny -> JSString -> JSAny -> IO ()
__set _ _ _ = return ()
__lst2arr :: Ptr [a] -> JSAny
__lst2arr _ = undefined
__arr2lst :: Int -> JSAny -> Ptr [a]
__arr2lst _ _ = undefined
jsString :: JSAny -> JSString
jsString _ = undefined
jsNumber :: JSAny -> Double
jsNumber _ = undefined
jsNull, jsTrue, jsFalse :: JSAny
jsNull = undefined
jsTrue = undefined
jsFalse = undefined
#endif
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
mkObj :: [(JSString, JSAny)] -> JSAny
mkObj ps = unsafePerformIO $ do
  o <- __new
  mapM_ (uncurry $ __set o) ps
  return o

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
  toAny x = treeToAny (isEnum g) $! gToAny 1 g
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
  toAny True  = jsTrue
  toAny False = jsFalse

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
instance FromAny Float where
  fromAny x = return (unsafeCoerce (jsNumber x))
instance FromAny Double where
  fromAny x = return (jsNumber x)
instance FromAny Char where
  fromAny x = return (unsafeCoerce (jsNumber x))
  listFromAny x = fromJSStr <$> fromAny x
instance FromAny () where
  fromAny _ = return ()
instance FromAny (Opaque a) where
  fromAny x = Opaque . fromPtr <$> fromAny x
instance FromAny Bool where
  fromAny x | x == jsTrue = return True
            | otherwise   = return False

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



-- Generic instances

-- | A field identifier: either a record selector, a constructor tag, or
--   nothing.
data Ident = None | Tag | Name !JSString

-- | JS object tree structure.
data JSTree = Leaf !JSAny | Tree ![(Ident, JSTree)]

-- | Merge two trees.
plus :: JSTree -> JSTree -> JSTree
plus (Tree a) (Tree b) = Tree (a ++ b)
plus (Tree a) b        = Tree (a ++ [(None, b)])
plus a (Tree b)        = Tree ((None, a) : b)
plus a b               = Tree [(None, a), (None, b)]

class GToAny f where
  gToAny :: Int -> f a -> JSTree
  isEnum :: f a -> Bool

instance GToAny U1 where
  gToAny _ U1 = error "U1: unpossible!"
  isEnum _    = True

instance ToAny a => GToAny (K1 i a) where
  gToAny _ (K1 x) = Leaf $ toAny x
  isEnum _ = False

instance (Selector c, GToAny a) => GToAny (M1 S c a) where
  gToAny cs (M1 x) =
    case selName (undefined :: M1 S c a ()) of
      "" -> Tree [(None, gToAny cs x)]
      n  -> Tree [(Name $ toJSStr n, gToAny cs x)]
  isEnum _ = isEnum (undefined :: a ())

instance Constructor c => GToAny (M1 C c U1) where
  gToAny _ _ = Leaf $ toAny $ conName (undefined :: M1 C c U1 ())
  isEnum _ = True

instance (Constructor c, GToAny a) => GToAny (M1 C c a) where
  gToAny cs (M1 x)
    | cs > 1 =
      Tree [(Tag, Leaf $ toAny tag)] `plus` gToAny cs x
    | otherwise =
      gToAny cs x
    where tag = conName (undefined :: M1 C c a ())
  isEnum _ = isEnum (undefined :: a ())

instance GToAny a => GToAny (M1 D c a) where
  gToAny cs (M1 x) = gToAny cs x
  isEnum _ = isEnum (undefined :: a ())

instance (GToAny a, GToAny b) => GToAny (a :*: b) where
  gToAny cs (a :*: b) = gToAny cs a `plus` gToAny cs b
  isEnum _ = False

instance (GToAny a, GToAny b) => GToAny (a :+: b) where
  gToAny cs (L1 x) = gToAny (cs+1) x
  gToAny cs (R1 x) = gToAny (cs+1) x
  isEnum _ = isEnum (undefined :: a ()) && isEnum (undefined :: b ())

-- | Build a JS object from a tree structure.
{-# NOINLINE treeToAny #-}
treeToAny :: Bool -> JSTree -> JSAny
treeToAny _ (Leaf x) = x
treeToAny inlineTags (Tree xs) =
  case xs of
    [(Tag, Leaf x)]
      | inlineTags      -> x
    xs'
      | all hasName xs' -> mkObj $ map (toKVPair inlineTags) xs'
      | otherwise       -> mkArr inlineTags xs'

mkArr :: Bool -> [(Ident, JSTree)] -> JSAny
mkArr inlineTags xs =
  case xs of
    ((Tag, t) : xs') -> mkObj [("$tag", treeToAny inlineTags t), contents xs']
    _                -> mkObj [contents xs]
  where
    contents xs' = ("$data", toAny $ map (treeToAny inlineTags . snd) xs')

toKVPair :: Bool -> (Ident, JSTree) -> (JSString, JSAny)
toKVPair inlineTags (Name n, x) = (n, treeToAny inlineTags x)
toKVPair inlineTags (Tag, x)    = ("$tag", treeToAny inlineTags x)
toKVPair _ _                    = error "Unpossible!"

hasName :: (Ident, b) -> Bool
hasName (None, _) = False
hasName _         = True
