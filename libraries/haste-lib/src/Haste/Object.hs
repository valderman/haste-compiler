{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances,
             MagicHash, GeneralizedNewtypeDeriving #-}
-- | Dealing with JavaScript objects on a low, low level.
module Haste.Object (
    JSObj, Type (..), Build,
    (#), asString, asBool, asNumber, typeOf, update, newObj, lookupPath, mkObj
  ) where
import Haste.Prim
import Haste.Foreign
import Control.Applicative
import System.IO.Unsafe

-- | A JS object: either null/undefined or 'Just' an actual value.
type JSObj = Maybe JSAny

-- | Possible types of JS objects.
data Type = TUndefined | TNumber | TBoolean | TString | TFunction | TObject
  deriving (Show, Eq, Enum)

instance FromAny Type where
  fromAny = toEnum . fromAny

-- | Any type on which we can look up a JS property.
class JSLookup a where
  infixl 4 #
  -- | Look up a property on an object-like value.
  (#) :: a -> JSString -> IO JSObj

instance JSLookup JSObj where
  Just o # prop = look o prop
  _      # _    = return Nothing

instance JSLookup a => JSLookup (IO a) where
  o # prop = o >>= (# prop)

-- | Lookup a whole path at once. More efficient for long paths.
--   @x `lookupPath` ["a", "b"]@ is equivalent to @x.a.b@.
lookupPath :: JSObj -> [JSString] -> IO JSObj
lookupPath = ffi "(function(o,as){\
                 \  for(var i in as){\
                 \    o = o[as[i]];\
                 \    if(typeof o==='undefined'){return null;}\
                 \  }\
                 \  return o;})"

look :: JSAny -> JSString -> IO (Maybe JSAny)
look = ffi "(function(o,s){return o[s] === undefined ? null : o[s];})"

-- | Convert the object to a 'JSString'.
asString :: JSObj -> IO (Maybe JSString)
asString = maybe (return Nothing) go
  where
    go = ffi "(function(o){return String(o);})"

-- | Convert the object to a 'Bool'.
asBool :: JSObj -> IO (Maybe Bool)
asBool = maybe (return Nothing) go
  where
    go = ffi "(function(o){return Boolean(o);})"

-- | Convert the object to a 'Double'.
asNumber :: JSObj -> IO (Maybe Double)
asNumber = maybe (return Nothing) go
  where
    go = ffi "(function(o){return Number(o);})"

-- | Get the type of a JS object.
typeOf :: JSObj -> IO Type
typeOf = maybe (return TUndefined) go
  where
    {-# NOINLINE go #-}
    go = ffi "(function(o){\
             \  switch(typeof o){\
             \    case 'undefined': return 0;\
             \    case 'number':    return 1;\
             \    case 'boolean':   return 2;\
             \    case 'string':    return 3;\
             \    case 'function':  return 4;\
             \    default:          return 5;\
             \  }\
             \})"

-- | Update a property on an object. Raise an error if object is null or not
--   an object type.
update :: ToAny a => JSObj -> JSString -> a -> Build ()
update o i x = Build $ update' o i x

-- | Update a property on an object. Raise an error if object is null or not
--   an object type.
update' :: ToAny a => JSObj -> JSString -> a -> IO ()
update' o i x = do
    t <- typeOf o
    case t of
      TObject   -> go o i (toAny x)
      TFunction -> go o i (toAny x)
      _         -> error "Tried to update non-object!"
  where
    go :: JSObj -> JSString -> JSAny -> IO ()
    go = ffi "(function(o,i,x){o[i]=x;})"

-- | Create a new, empty JS object.
newObj :: IO JSObj
newObj = ffi "(function(){return {};})"

-- | Create a new JS object.
{-# NOINLINE mkObj #-}
mkObj :: (JSObj -> Build ()) -> JSObj
mkObj f = unsafePerformIO $ do
  o <- newObj
  unBuild $ f o
  return o

-- | JS object builder monad. Essentially just prevents random IO.
newtype Build a = Build {unBuild :: IO a}
  deriving (Functor, Applicative, Monad)
