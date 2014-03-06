{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, CPP #-}
-- | Low level XMLHttpRequest support. IE6 and older are not supported.
module Haste.Ajax (Method (..), URL, Key, Val, textRequest, textRequest_,
                   jsonRequest, jsonRequest_) where
import Haste.Prim
import Haste.Callback
import Haste.JSON
import Control.Monad.IO.Class

#ifdef __HASTE__
foreign import ccall ajaxReq :: JSString    -- method
                             -> JSString    -- url
                             -> Bool        -- async?
                             -> JSString    -- POST data
                             -> JSFun (Maybe JSString -> IO ())
                             -> IO ()
#else
ajaxReq :: JSString -> JSString -> Bool -> JSString -> JSFun (Maybe JSString -> IO ()) -> IO ()
ajaxReq = error "Tried to use ajaxReq in native code!"
#endif

data Method = GET | POST deriving Show
type Key = String
type Val = String

-- | Make an AJAX request to a URL, treating the response as plain text.
textRequest :: MonadIO m
            => Method
            -> URL
            -> [(Key, Val)]
            -> (Maybe String -> IO ())
            -> m ()
textRequest m url kv cb = do
  _ <- liftIO $ ajaxReq (toJSStr $ show m) url' True "" cb'
  return ()
  where
    cb' = mkCallback $ cb . fmap fromJSStr
    kv' = map (\(k,v) -> (toJSStr k, toJSStr v)) kv
    url' = if null kv
             then toJSStr url
             else catJSStr "?" [toJSStr url, toQueryString kv']

-- | Same as 'textRequest' but deals with JSStrings instead of Strings.
textRequest_ :: MonadIO m
             => Method
             -> JSString
             -> [(JSString, JSString)]
             -> (Maybe JSString -> IO ())
             -> m ()
textRequest_ m url kv cb = liftIO $ do
  _ <- ajaxReq (toJSStr $ show m) url' True "" (mkCallback cb)
  return ()
  where
    url' = if null kv then url else catJSStr "?" [url, toQueryString kv]

-- | Make an AJAX request to a URL, interpreting the response as JSON.
jsonRequest :: MonadIO m
            => Method
            -> URL
            -> [(Key, Val)]
            -> (Maybe JSON -> IO ())
            -> m ()
jsonRequest m url kv cb = liftIO $ do
  jsonRequest_ m (toJSStr url)
                 (map (\(k,v) -> (toJSStr k, toJSStr v)) kv)
                 cb

-- | Does the same thing as 'jsonRequest' but uses 'JSString's instead of
--   Strings.
jsonRequest_ :: MonadIO m 
             => Method
             -> JSString
             -> [(JSString, JSString)]
             -> (Maybe JSON -> IO ())
             -> m ()
jsonRequest_ m url kv cb = liftIO $ do
  _ <- ajaxReq (toJSStr $ show m) url' True "" cb'
  return ()
  where
    liftEither (Right x) = Just x
    liftEither _         = Nothing
    cb' = mkCallback $ \mjson -> cb (mjson >>= liftEither . decodeJSON)
    url' = if null kv then url else catJSStr "?" [url, toQueryString kv]

toQueryString :: [(JSString, JSString)] -> JSString
toQueryString = catJSStr "&" . map (\(k,v) -> catJSStr "=" [k,v])
