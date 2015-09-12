{-# LANGUAGE OverloadedStrings #-}
-- | Low level XMLHttpRequest support. IE6 and older are not supported.
module Haste.Ajax (Method (..), URL, ajaxRequest, noParams) where
import Haste.Foreign
import Haste.Prim
import Haste.Prim.JSType
import Control.Monad.IO.Class
import Control.Monad (join)

ajaxReq :: Method   -- method (GET/POST)
        -> JSString -- URL
        -> Bool     -- async?
        -> JSString -- POST data
        -> (Maybe JSString -> IO ()) -- callback
        -> IO ()
ajaxReq = ffi "(function(method, url, async, postdata, cb) {\
    \var xhr = new XMLHttpRequest();\
    \xhr.open(method, url, async);\
    \if(method == 'POST') {\
        \xhr.setRequestHeader('Content-type',\
                             \'application/x-www-form-urlencoded');\
    \}\
    \xhr.onreadystatechange = function() {\
        \if(xhr.readyState == 4) {\
            \cb(xhr.status == 200 ? xhr.responseText : null);\
        \}\
    \};\
    \xhr.send(postdata);})"

data Method = GET | POST deriving Show

instance ToAny Method where
  toAny GET  = toAny ("GET" :: JSString)
  toAny POST = toAny ("POST" :: JSString)

-- | Pass to 'ajaxRequest' instead of @[]@ when no parameters are needed, to
--   avoid type ambiguity errors.
noParams :: [((), ())]
noParams = []

-- | Perform an AJAX request.
ajaxRequest :: (MonadIO m, JSType a, JSType b, JSType c)
            => Method   -- ^ GET or POST. For GET, pass all params in URL.
                        --   For POST, pass all params as post data.
            -> URL      -- ^ URL to make AJAX request to.
            -> [(a, b)] -- ^ A list of (key, value) parameters.
            -> (Maybe c -> IO ()) -- ^ Callback to invoke on completion.
            -> m ()
ajaxRequest m url kv cb = liftIO $ do
    _ <- ajaxReq m url' True pd (cb . join . fmap fromJSString)
    return ()
  where
    url' = case m of
           GET
             | null kv   -> toJSString url
             | otherwise -> catJSStr "?" [toJSString url, toQueryString kv]
           POST -> toJSString url
    pd = case m of
           GET -> ""
           POST
             | null kv   -> ""
             | otherwise -> toQueryString kv

toQueryString :: (JSType a, JSType b) =>[(a, b)] -> JSString
toQueryString = catJSStr "&" . map f
  where f (k, v) = catJSStr "=" [toJSString k,toJSString v]
