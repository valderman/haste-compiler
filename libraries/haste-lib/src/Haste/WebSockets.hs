{-# LANGUAGE FlexibleInstances, EmptyDataDecls #-}
-- | WebSockets API for Haste.
module Haste.WebSockets (
    module Haste.Concurrent,
    WebSocket,
    withWebSocket, wsSend
  ) where
import Haste
import Haste.Concurrent
import Unsafe.Coerce

data WebSocket
instance Marshal WebSocket where
  pack = unsafeCoerce
  unpack = unsafeCoerce
instance Marshal (WebSocket -> JSString -> IO ()) where
  pack = unsafeCoerce
  unpack = unsafeCoerce
instance Marshal (WebSocket -> IO ()) where
  pack = unsafeCoerce
  unpack = unsafeCoerce
instance Marshal (IO ()) where
  pack = unsafeCoerce
  unpack = unsafeCoerce

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

new :: URL
    -> (WebSocket -> JSString -> IO ())
    -> (WebSocket -> IO ())
    -> IO ()
    -> IO ()
new = ffi "(function(url, cb, f, err) {\
             \var ws = new WebSocket(url);\
             \ws.onmessage = function(e) {A(cb,[ws, [0,e.data],0]);};\
             \ws.onopen = function(e) {A(f,[ws,0]);};\
             \ws.onerror = function(e) {A(err,[0]);};\
             \return ws;\
           \})" 

-- | Send a string over a WebSocket.
wsSend :: WebSocket -> JSString -> CIO ()
wsSend ws str = liftIO $ send ws str

send :: WebSocket -> JSString -> IO ()
send = ffi "(function(s, msg) {s.send(msg); return 0;})"
