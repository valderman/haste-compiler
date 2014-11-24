{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}
-- | Concurrent Ajax calls. IE6 and older are not supported.
module Haste.Concurrent.Ajax (
    Method (..), URL, noParams,
    Haste.Concurrent.Ajax.ajaxRequest
  ) where
import Haste.Concurrent.Monad
import Haste.Ajax
import Haste.JSType

-- | Make a blocking AJAX request in the CIO monad.
--   Note that unlike using the browser's native blocking AJAX facilities,
--   this does *not* freeze the browser.
ajaxRequest :: (JSType a, JSType b, JSType c)
            => Method   -- ^ GET or POST. For GET, pass all params in URL.
                        --   For POST, pass all params as post data.
            -> URL      -- ^ URL to make AJAX request to.
            -> [(a, b)] -- ^ A list of (key, value) parameters.
            -> CIO (Maybe c)
ajaxRequest method url kv = do
  v <- newEmptyMVar
  liftIO $ Haste.Ajax.ajaxRequest method url kv $ concurrent . putMVar v
  takeMVar v
