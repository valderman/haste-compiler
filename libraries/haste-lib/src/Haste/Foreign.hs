{-# LANGUAGE CPP #-}
-- | High level JavaScript foreign interface.
module Haste.Foreign (
    -- * Conversion to/from JSAny
    ToAny (..), FromAny (..), JSAny,
    Opaque, toOpaque, fromOpaque,
    nullValue, toObject, has, get, index,
    getMaybe, hasAll, lookupAny

    -- * Importing and exporting JavaScript functions
    FFI, JSFunc,
    ffi, constant, export
#if __GLASGOW_HASKELL__ >= 710
    , safe_ffi, StaticPtr
#endif
  ) where
import Haste.Prim.Foreign
import Haste.Prim (JSString)
import Control.Monad (foldM)

-- | Read a member from a JS object. Succeeds if the member exists.
getMaybe :: (FromAny a) => JSAny -> JSString -> IO (Maybe a)
getMaybe a k = do exists <- has a k
                  if exists then Just <$> get a k
                    else pure Nothing

-- | Checks if a JS object has a list of members. Succeeds if the JS object
--   has every member given in the list. 
hasAll :: JSAny -> [JSString] -> IO Bool
hasAll a ks = and <$> mapM (has a) ks


-- | Looks up an object by a `.`-separated string. Succeeds if the lowest
--   member exists.
--
-- Usage example:
-- 
-- >>> {'child': {'childrer': {'childerest': "I am very much a child."}}}
--
-- Given the JS Object above, we can access the deeply nested object,
--  childerest, by lookupAny as in the below example
--
-- >>> lookupAny jsObject "child.childrer.childerest"
lookupAny :: JSAny -> JSString -> IO (Maybe JSAny)
lookupAny root i = foldM hasGet (Just root) $ J.match (J.regex "[^.]+" "g") i
  where hasGet (Just parent) id = do h <- has parent id
                                     if h then Just <$> get parent id
                                       else pure Nothing
