{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
-- | Low level XMLHttpRequest support. IE6 and older are not supported.
module Haste.Ajax (Method (..), URL, ajaxRequest, noParams) where
import Haste.Prim
import Haste.Prim.JSType
import Control.Monad.IO.Class

#ifdef __HASTE__
foreign import ccall ajaxReq :: JSString    -- method
                             -> JSString    -- url
                             -> Bool        -- async?
                             -> JSString    -- POST data
                             -> Ptr (Maybe JSString -> IO ())
                             -> IO ()
#else
ajaxReq :: JSString -> JSString -> Bool -> JSString -> Ptr (Maybe JSString -> IO ()) -> IO ()
ajaxReq = error "Tried to use ajaxReq in native code!"
#endif

data Method = GET | POST deriving Show

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
    _ <- ajaxReq (showm m) url' True pd cb'
    return ()
  where
    showm GET  = "GET"
    showm POST = "POST"
    cb' = toPtr $ cb . fromJSS
    fromJSS (Just jss) = fromJSString jss
    fromJSS _          = Nothing
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
