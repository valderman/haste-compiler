{-# LANGUAGE ForeignFunctionInterface, PatternGuards, CPP #-}
-- | Converting to/from JS-native data.
module Haste.Any (
    ToAny (..), FromAny (..), JSAny,
    Opaque, toOpaque, fromOpaque
  ) where
import Haste.Prim
import Haste.JSType
import Data.Int
import Data.Word
import Unsafe.Coerce

#ifdef __HASTE__
foreign import ccall __lst2arr :: Ptr [a] -> JSAny
foreign import ccall __arr2lst :: Int -> JSAny -> Ptr [a]
foreign import ccall "String" jsString :: JSAny -> JSString
foreign import ccall "Number" jsNumber :: JSAny -> Double
foreign import ccall "__jsNull" jsNull :: JSAny
foreign import ccall "__jsTrue" jsTrue :: JSAny
foreign import ccall "__jsFalse" jsFalse :: JSAny
#else
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

-- | Any type that can be converted into a JavaScript value.
class ToAny a where
  toAny :: a -> JSAny

  listToAny :: [a] -> JSAny
  listToAny = __lst2arr . toPtr . map toAny

-- | Any type that can be converted from a JavaScript value.
class FromAny a where
  -- | Convert a value from JS with a reasonable conversion if an exact match
  --   is not possible. Examples of reasonable conversions would be truncating
  --   floating point numbers to integers, or turning signed integers into
  --   unsigned.
  fromAny :: JSAny -> a

  listFromAny :: JSAny -> [a]
  listFromAny = map fromAny . fromPtr . __arr2lst 0

-- | The Opaque type is inhabited by values that can be passed to JavaScript
--   using their raw Haskell representation. Opaque values are completely
--   useless to JS code, and should not be inspected. This is useful for,
--   for instance, storing data in some JS-native data structure for later
--   retrieval.
newtype Opaque a = Opaque {fromOpaque :: a}

toOpaque :: a -> Opaque a
toOpaque = Opaque



-- ToAny instances
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



-- FromAny instances
fromNum :: JSAny -> Double
fromNum x =
  case jsNumber x of
    x' | isNaN x'  -> error "Tried to convert non-number into a Double!"
       | otherwise -> x'

instance FromAny (Ptr a) where
  fromAny = unsafeCoerce
instance FromAny JSString where
  fromAny = jsString
instance FromAny Int where
  fromAny = convert . fromNum
instance FromAny Int8 where
  fromAny = convert . fromNum
instance FromAny Int16 where
  fromAny = convert . fromNum
instance FromAny Int32 where
  fromAny = convert . fromNum
instance FromAny Word where
  fromAny = convert . fromNum
instance FromAny Word8 where
  fromAny = convert . fromNum
instance FromAny Word16 where
  fromAny = convert . fromNum
instance FromAny Word32 where
  fromAny = convert . fromNum
instance FromAny Float where
  fromAny = unsafeCoerce . fromNum
instance FromAny Double where
  fromAny = fromNum
instance FromAny Char where
  fromAny = unsafeCoerce . fromNum
  listFromAny = fromJSStr . fromAny
instance FromAny () where
  fromAny _ = ()
instance FromAny (Opaque a) where
  fromAny x = Opaque $ fromPtr $ fromAny x
instance FromAny Bool where
  fromAny x | x == jsTrue = True
            | otherwise   = False

instance FromAny a => FromAny [a] where
  fromAny = listFromAny

instance FromAny a => FromAny (Maybe a) where
  fromAny x | x == jsNull = Nothing
            | otherwise   = Just $ fromAny x

instance (FromAny a, FromAny b) => FromAny (a, b) where
  fromAny x | [a,b] <- fromAny x = (fromAny a,fromAny b)
            | otherwise = error "Tried to fromAny tuple with wrong size!"

instance (FromAny a, FromAny b, FromAny c) => FromAny (a, b, c) where
  fromAny x | [a,b,c] <- fromAny x = (fromAny a,fromAny b,fromAny c)
            | otherwise = error "Tried to fromAny tuple with wrong size!"

instance (FromAny a, FromAny b, FromAny c, FromAny d) =>
          FromAny (a, b, c, d) where
  fromAny x | [a,b,c,d] <- fromAny x = (fromAny a,fromAny b,fromAny c,fromAny d)
            | otherwise = error "Tried to fromAny tuple with wrong size!"

instance (FromAny a, FromAny b, FromAny c, FromAny d, FromAny e) =>
          FromAny (a, b, c, d, e) where
  fromAny x | [a,b,c,d,e] <- fromAny x =
              (fromAny a,fromAny b,fromAny c,fromAny d,fromAny e)
            | otherwise = error "Tried to fromAny tuple with wrong size!"

instance (FromAny a, FromAny b, FromAny c, FromAny d, FromAny e, FromAny f) =>
          FromAny (a, b, c, d, e, f) where
  fromAny x | [a,b,c,d,e,f] <- fromAny x =
              (fromAny a,fromAny b,fromAny c,fromAny d,fromAny e,fromAny f)
            | otherwise = error "Tried to fromAny tuple with wrong size!"

instance (FromAny a, FromAny b, FromAny c, FromAny d,
          FromAny e, FromAny f, FromAny g) =>
          FromAny (a, b, c, d, e, f, g) where
  fromAny x | [a,b,c,d,e,f,g] <- fromAny x =
              (fromAny a, fromAny b, fromAny c, fromAny d,
               fromAny e, fromAny f, fromAny g)
            | otherwise = error "Tried to fromAny tuple with wrong size!"
