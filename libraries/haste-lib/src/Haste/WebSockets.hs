{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving#-}
-- | WebSockets API for Haste.
module Haste.WebSockets (
    module Haste.Concurrent,
    WebSocket,
    withWebSocket, withBinaryWebSocket, wsSend, wsSendBlob
  ) where
import Haste
import Haste.Prim.Foreign
import Haste.Concurrent
import Haste.Binary (Blob)

newtype WebSocket = WebSocket JSAny deriving (ToAny, FromAny)

-- | Run a computation with a web socket. The computation will not be executed
--   until a connection to the server has been established.
withWebSocket :: URL
              -- ^ URL to bind the WebSocket to
              -> (WebSocket -> JSString -> CIO ())
              -- ^ Computation to run when new data arrives
              -> CIO a
              -- ^ Computation to run when an error occurs
              -> (WebSocket -> CIO a)
              -- ^ Computation using the WebSocket
              -> CIO a
withWebSocket url cb err f = do
    result <- newEmptyMVar
    let f' = \ws -> concurrent $ f ws >>= putMVar result
    liftIO $ new url cb' f' $ concurrent $ err >>= putMVar result
    takeMVar result
  where
    cb' = \ws msg -> concurrent $ cb ws msg

-- | Run a computation with a web socket. The computation will not be executed
--   until a connection to the server has been established.
withBinaryWebSocket :: URL
              -- ^ URL to bind the WebSocket to
              -> (WebSocket -> Blob -> CIO ())
              -- ^ Computation to run when new data arrives
              -> CIO a
              -- ^ Computation to run when an error occurs
              -> (WebSocket -> CIO a)
              -- ^ Computation using the WebSocket
              -> CIO a
withBinaryWebSocket url cb err f = do
    result <- newEmptyMVar
    let f' = \ws -> concurrent $ f ws >>= putMVar result
    liftIO $ newBin url cb' f' $ concurrent $ err >>= putMVar result
    takeMVar result
  where
    cb' = \ws msg -> concurrent $ cb ws msg

new :: URL
    -> (WebSocket -> JSString -> IO ())
    -> (WebSocket -> IO ())
    -> IO ()
    -> IO ()
new = ffi "(function(url, cb, f, err) {\
             \var ws = new WebSocket(url);\
             \ws.onmessage = function(e) {cb(ws,e.data);};\
             \ws.onopen = function(e) {f(ws);};\
             \ws.onerror = function(e) {err());};\
             \return ws;\
           \})" 

newBin :: URL
       -> (WebSocket -> Blob -> IO ())
       -> (WebSocket -> IO ())
       -> IO ()
       -> IO ()
newBin = ffi "(function(url, cb, f, err) {\
                \var ws = new WebSocket(url);\
                \ws.binaryType = 'blob';\
                \ws.onmessage = function(e) {cb(ws,e.data);};\
                \ws.onopen = function(e) {f(ws);};\
                \ws.onerror = function(e) {err();};\
                \return ws;\
              \})" 

-- | Send a string over a WebSocket.
wsSend :: WebSocket -> JSString -> CIO ()
wsSend ws str = liftIO $ sendS ws str

-- | Send a Blob over a WebSocket.
wsSendBlob :: WebSocket -> Blob -> CIO ()
wsSendBlob ws b = liftIO $ sendB ws b

sendS :: WebSocket -> JSString -> IO ()
sendS = ffi "(function(s, msg) {s.send(msg);})"

sendB :: WebSocket -> Blob -> IO ()
sendB = ffi "(function(s, msg) {s.send(msg);})"
