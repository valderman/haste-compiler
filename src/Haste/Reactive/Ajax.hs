{-# LANGUAGE OverloadedStrings #-}
module Haste.Reactive.Ajax (jsonSig, ajaxSig) where
import Haste.Reactive.Signal
import Haste.Ajax
import Haste.JSON
import Control.Applicative

-- | Create a signal for JSON request. When triggered, the signal causes an
--   XMLHttpRequest to be made to the given URL, with a query string built from
--   the given (key, value) pairs. It notifies its listeners after the request
--   returns.
--   If no data was received, or if the data wasn't JSON, don't notify
--   listeners.
jsonSig :: Signal URL -> Signal [(Key, Val)] -> Signal JSON
jsonSig requrl kvs = async (ajax <$> requrl <*> kvs)
  where
    ajax url kv p = do
      jsonRequest GET url kv $ \mjson ->
        case mjson of
          Just json -> push json p
          _         -> return ()

-- | Create a signal for a generic AJAX request. It works just like 'jsonSig',
--   but returns its output as plain text rather than JSON.
ajaxSig :: Signal URL -> Signal [(Key, Val)] -> Signal String
ajaxSig requrl kvs = async (ajax <$> requrl <*> kvs)
  where
    ajax url kv p = do
      textRequest GET url kv $ \text -> do
        push text p
