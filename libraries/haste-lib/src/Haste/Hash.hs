{-# LANGUAGE FlexibleInstances, FlexibleContexts, GADTs, OverloadedStrings #-}
-- | Hash manipulation and callbacks.
module Haste.Hash (
    onHashChange, onHashChange', setHash, getHash, setHash', getHash'
  ) where
import Haste.Foreign
import Control.Monad.IO.Class
import Haste.Callback
import Haste.Prim
import Unsafe.Coerce

newtype HashCallback = HashCallback (JSString -> JSString -> IO ())

instance Marshal HashCallback where
  pack = unsafeCoerce
  unpack = unsafeCoerce

-- | Register a callback to be run whenever the URL hash changes.
--   The two arguments of the callback are the new and old hash respectively.
onHashChange :: (MonadIO m, GenericCallback (m ()) m, CB (m ()) ~ IO ())
              => (String -> String -> m ())
              -> m ()
onHashChange f = do
    firsthash <- getHash'
    f' <- toCallback $ \old new -> f (fromJSStr old) (fromJSStr new)
    liftIO $ jsOnHashChange firsthash (HashCallback f')

-- | JSString version of @onHashChange@.
onHashChange' :: (MonadIO m, GenericCallback (m ()) m, CB (m ()) ~ IO ())
              => (JSString -> JSString -> m ())
              -> m ()
onHashChange' f = do
    firsthash <- getHash'
    f' <- toCallback f
    liftIO $ jsOnHashChange firsthash (HashCallback f')

{-# NOINLINE jsOnHashChange #-}
jsOnHashChange :: JSString -> HashCallback -> IO ()
jsOnHashChange =
  ffi "(function(firsthash,cb){\
          \window.__old_hash = firsthash;\
          \window.onhashchange = function(e){\
            \var oldhash = window.__old_hash;\
            \var newhash = window.location.hash.split('#')[1] || '';\
            \window.__old_hash = newhash;\
            \A(cb, [[0,oldhash],[0,newhash],0]);\
          \};\
       \})"

-- | Set the hash part of the current URL.
setHash :: MonadIO m => String -> m ()
setHash = liftIO . jsSetHash . toJSStr

-- | Set the hash part of the current URL - JSString version.
setHash' :: MonadIO m => JSString -> m ()
setHash' = liftIO . jsSetHash

{-# NOINLINE jsSetHash #-}
jsSetHash :: JSString -> IO ()
jsSetHash = ffi "(function(h) {location.hash = '#'+h;})"

-- | Read the hash part of the currunt URL.
getHash :: MonadIO m => m String
getHash = liftIO $ fromJSStr `fmap` jsGetHash

-- | Read the hash part of the currunt URL - JSString version.
getHash' :: MonadIO m => m JSString
getHash' = liftIO jsGetHash

{-# NOINLINE jsGetHash #-}
jsGetHash :: IO JSString
jsGetHash = ffi "(function() {return location.hash.substring(1);})"
