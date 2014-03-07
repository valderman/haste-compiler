{-# LANGUAGE OverloadedStrings #-}
-- | Basic bindings to HTML5 WebStorage.
module Haste.LocalStorage (setItem, getItem, removeItem) where
import Haste
import Haste.Foreign
import Haste.Serialize
import Haste.JSON
import Control.Applicative

-- | Locally store a serializable value.
setItem :: Serialize a => String -> a -> IO ()
setItem k = store k . encodeJSON . toJSON
  where
    store :: String -> JSString -> IO ()
    store = ffi "(function(k,v) {localStorage.setItem(k,v);})"

-- | Load a serializable value from local storage. Will fail if the given key
--   does not exist or if the value stored at the key does not match the
--   requested type.
getItem :: Serialize a => String -> IO (Either String a)
getItem k = do
    maybe (Left "No such value") (\s -> decodeJSON s >>= fromJSON) <$> load k
  where
    load :: String -> IO (Maybe JSString)
    load = ffi "(function(k) {return localStorage.getItem(k);})"

-- | Remove a value from local storage.
removeItem :: String -> IO ()
removeItem = ffi "(function(k) {localStorage.removeItem(k);})"
