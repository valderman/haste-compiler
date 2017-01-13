{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables #-}
-- | XMLHttpRequest support. IE9 and older are not supported.
module Haste.Ajax
  ( Method (..)
  , AjaxData (..)
  , AjaxError (..)
  , ajax, ajaxWithMime
  ) where
import Haste.Prim.Foreign
import Haste.Prim
import Haste.Prim.JSType
import Control.Monad.IO.Class
import Control.Monad (join)
import Haste.Concurrent
import Data.Proxy
import Haste.Binary hiding (get)

class (ToAny a, FromAny a) => AjaxData a where
  -- | The MIME type of the data represented by this type.
  mimeType     :: Proxy a -> JSString

  -- | The XHR response type that corresponds to this type.
  responseType :: Proxy a -> JSString

instance AjaxData JSString where
  mimeType _ = ""
  responseType _ = ""

instance AjaxData Blob where
  mimeType _ = "application/octet-stream"
  responseType _ = "blob"

instance AjaxData () where
  mimeType _ = ""
  responseType _ = ""

-- | Make an AJAX request. The return value is either the body of the requested
--   document, or an error.
ajax :: forall m post resp.
       (MonadConc m, AjaxData post, AjaxData resp)
     => Method post -- ^ HTTP method to use for request. @post@ is always @()@
                    --   for GET requests.
     -> URL         -- ^ URL to request.
     -> m (Either AjaxError resp)
ajax = ajaxWithMime ""

-- | Like 'ajax', but accepts a custom MIME type for POST data.
--   Use this with the appropriate MIME type and POST data if you want to send,
--   for instance, form data.
ajaxWithMime :: forall m post resp.
       (MonadConc m, AjaxData post, AjaxData resp)
     => JSString    -- ^ MIME type of any POST data. Decided by the 'AjaxData'
                    --   instance for @post@ if the empty string is given.
                    --   Only relevant to POST requests.
     -> Method post -- ^ HTTP method to use for request.
     -> URL         -- ^ URL to request.
     -> m (Either AjaxError resp)
ajaxWithMime mime method url = do
    res <- newEmptyMVar
    liftIO $ ajaxReq methodStr url mime' respType postdata $ \merr md -> do
      case (md, merr) of
        (Just d, _)   -> fromAny d >>= concurrent . putMVar res . Right
        (_, Just err) -> concurrent $ putMVar res (Left err)
    liftConc $ takeMVar res
  where
    mime'
      | "" /= mime = mime
      | otherwise  = mimeType (Proxy :: Proxy post)
    respType = responseType (Proxy :: Proxy resp)
    (postdata, methodStr) = case method of
      GET    -> (Nothing, "GET")
      POST d -> (Just $ toAny d, "POST")

-- | An error which occurred during an AJAX request.
--   Might be either a network error (denied by CSP, host unreachable etc.)
--   or an HTTP error, with status code and description.
data AjaxError
  = NetworkError
  | HttpError Int JSString
    deriving (Show, Eq)

instance FromAny AjaxError where
  fromAny x = do
    errtype <- get x "type"
    case errtype of
      "network" -> pure NetworkError
      "http"    -> HttpError <$> get x "status" <*> get x "status-text"
      _         -> fail $ "unknown type of ajax error: " ++ fromJSStr errtype

-- | HTTP method to use for request. POST requests take an (optionally empty)
--   JSString representing data to POST.
data Method a where
  GET  :: Method ()
  POST :: a -> Method a

ajaxReq :: JSString -- ^ method (GET/POST)
        -> JSString -- ^ URI
        -> JSString -- ^ Outgoing MIME type; empty string means default
        -> JSString -- ^ responseType field
        -> Maybe JSAny -- ^ POST data
        -> (Maybe AjaxError -> Maybe JSAny -> IO ())
           -- ^ Callback; if successful, first argument is 0, the second the
           --   empty string, and the third the response data.
           --   If not, third argument is null and the other two give
           --   HTTP status and error message.
        -> IO ()
ajaxReq = ffi "(function(method, uri, mimeout, responseType, postdata, cb) {\
    \var xhr = new XMLHttpRequest();\
    \xhr.open(method, uri);\
    \xhr.responseType = responseType;\
    \if(mimeout != '') {\
      \xhr.setRequestHeader('Content-type', mimeout);\
    \}\
    \xhr.addEventListener('load', function() {\
      \if(xhr.status < 400) {cb(null, xhr.response);}\
      \else {cb({'type':'http', 'status':xhr.status, 'status-text': xhr.statusText}, null);}\
    \});\
    \xhr.addEventListener('error', function() {\
      \if(xhr.status != 0) {\
        \cb({'type':'http', 'status':xhr.status, 'status-text': xhr.statusText}, null);\
      \} else {\
        \cb({'type':'network'}, null);\
      \}\
    \});\
    \xhr.send(postdata);\
  \})"
