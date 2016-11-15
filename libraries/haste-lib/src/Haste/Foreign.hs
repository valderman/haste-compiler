{-# LANGUAGE CPP, OverloadedStrings #-}
-- | High level JavaScript foreign interface.
module Haste.Foreign
  ( -- * Conversion to/from JSAny
    ToAny (..), FromAny (..), JSAny
  , Opaque, toOpaque, fromOpaque
  , nullValue, toObject, has, get, index, isUndefined
  , getMaybe, hasAll, lookupAny, JSException (..)

    -- * Importing and exporting JavaScript functions
  , FFI, JSFunc
  , ffi, constant, export
  , safe_ffi, StaticPtr
  , withLibraries
  ) where
import Haste.Prim.Foreign
import Haste.Prim (JSString)
import qualified Haste.JSString as J
import Control.Monad (foldM)

-- For withLibraries
import Haste.Concurrent
import Haste.DOM.JSString
import Haste.Events

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
lookupAny root i = foldM hasGet (Just root) $ J.match dotsplit i
  where hasGet Nothing       _     = pure Nothing
        hasGet (Just parent) ident = do h <- has parent ident
                                        if h then Just <$> get parent ident
                                          else pure Nothing
        dotsplit = J.regex "[^.]+" "g"

-- | Wait for the given libraries to load before executing the given
--   computation. Note that this function returns immediately, BEFORE the given
--   libraries are loaded.
withLibraries :: [JSString] -> IO () -> IO ()
withLibraries libs m = concurrent $ do
    vars <- mapM (const newEmptyMVar) libs
    liftIO . sequence_ $ zipWith addLib libs vars
    mapM_ takeMVar vars
    liftIO m
  where
    addLib lib var = do
      s <- newElem "SCRIPT" `with` ["src" =: lib]
      s `onEvent` Load $ \_ -> concurrent $ putMVar var ()
      appendChild documentBody s
