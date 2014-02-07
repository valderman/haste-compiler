{-# LANGUAGE FlexibleInstances, FlexibleContexts, GADTs #-}
-- | Hash manipulation and callbacks.
module Haste.Hash (onHashChange, setHash, getHash) where
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
    f' <- toCallback $ \old new -> f (fromJSStr old) (fromJSStr new)
    liftIO $ go (HashCallback f')
  where
    go :: HashCallback -> IO ()
    go = ffi "(function(cb,_) {\
             \  window.onhashchange = function(e){\
             \      A(cb, [[0,e.oldURL.split('#')[1] || ''],\
             \             [0,e.newURL.split('#')[1] || ''],0]);\
             \    };\
             \})"

-- | Set the hash part of the current URL.
setHash :: MonadIO m => String -> m ()
setHash = liftIO . ffi "(function(h,_) {location.hash = '#'+h;})"

-- | Read the hash part of the currunt URL.
getHash :: MonadIO m => m String
getHash = liftIO $ ffi "(function(_) {return location.hash.substring(1);})"
