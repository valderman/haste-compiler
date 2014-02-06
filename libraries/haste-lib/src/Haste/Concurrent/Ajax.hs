{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}
-- | Concurrent Ajax calls. IE6 and older are not supported.
module Haste.Concurrent.Ajax (
    Key, Val, JSON (..), (!), (~>), Method (..), URL, AjaxData (..),
    ajaxRequest
  ) where
import Haste.Concurrent.Monad
import Haste.Ajax
import Haste.JSType
import Haste.JSON
import Haste.Prim (JSString)

-- | Data that can be received from an AJAX call.
class AjaxData a where
  encode :: a -> JSString
  decode :: JSString -> Maybe a

instance JSType a => AjaxData a where
  encode = toJSString
  decode = fromJSString

instance AjaxData JSON where
  encode = encodeJSON
  decode x =
    case decodeJSON x of
      Right x' -> Just x'
      _        -> Nothing

-- | Make an Ajax GET request to a URL. The function will block until a
--   response is received.
ajaxRequest :: AjaxData a
            => URL          -- ^ Base URL to call. The query string generated
                            --   from the second argument will be appended to
                            --   this to construct the final URL.
            -> [(Key, Val)] -- ^ Key value pairs to construct the query part
                            --   of the URL from.
                            --   Passing @[("foo","0"), ("bar", "1")]@ will
                            --   result in a request URL that ends with
                            --   @?foo=0&bar=1@.
            -> CIO (Maybe a)
ajaxRequest url querypart = do
    v <- newEmptyMVar
    let cb x = concurrent $ putMVar v $ x >>= decode
    liftIO $ textRequest_ GET url' querypart' cb
    takeMVar v
  where
    url' = toJSString url
    querypart' = [(toJSString k, toJSString v) | (k, v) <- querypart]
