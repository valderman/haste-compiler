{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleInstances #-}
-- | WebSockets API for Haste.
module Haste.WebSockets
  ( module Haste.Concurrent
  , WebSocket, WebSocketConfig (..), WebSocketCloseEvent (..)
  , WebSocketData (..)
  , noHandlers
  , wsOpenSync
  , wsClose
  , withWebSocket, withBinaryWebSocket, wsSendBlob
  ) where
import Haste
import Haste.Prim.Foreign
import Haste.Concurrent
import Haste.Binary (Blob)

-- | A WebSocket, parameterized over the type of data that can be sent and
--   received over it.
newtype WebSocket a = WebSocket JSAny deriving (ToAny, FromAny)

instance Eq (WebSocket a) where
  (WebSocket a) == (WebSocket b) = a == b

-- | Some data that can be sent and received over a WebSocket.
class WebSocketData a where
  -- | Send a piece of data to a WebSocket of the appropriate type.
  --   Returns @False@ if the data could not be sent, for instance due to a
  --   closed socket.
  wsSend :: WebSocket a -> a -> CIO Bool

  -- | Open a new WebSocket connection, returning @Just socket@ when successful.
  wsOpen :: WebSocketConfig a -> CIO (Maybe (WebSocket a))

instance WebSocketData Blob where
  wsSend ws b = liftIO $ sendB ws b
  wsOpen = open newBin

instance WebSocketData JSString where
  wsSend ws s = liftIO $ sendS ws s
  wsOpen = open new

open mkNew (WebSocketConfig url close msg) = do
  sock <- newEmptyMVar
  liftIO $ mkNew url
    (\s d -> concurrent $ msg s d)
    (concurrent . putMVar sock . Just)
    (concurrent . close)
    (concurrent $ putMVar sock Nothing)
  takeMVar sock

-- | The various event handlers supported by WebSockets.
data WebSocketConfig t = WebSocketConfig
  { -- | URL to which to attempt a WebSocket connection.
    wsOpenURL   :: URL
    -- | Called when the connection is closed by the server.
  , wsOnClose   :: WebSocketCloseEvent -> CIO ()
    -- | Called when a new message arrives.
  , wsOnMessage :: WebSocket t -> t -> CIO ()
  }

-- | Event describing the remote closing of a WebSocket.
data WebSocketCloseEvent = WebSocketCloseEvent
  { -- | Reason for close event, computer-readable version.
    wsCloseCode   :: Int
    -- | Reason for close event, human-readable version.
  , wsCloseReason :: JSString
    -- | Was the socket closed cleanly?
  , wsCloseClean  :: Bool
  }

instance FromAny WebSocketCloseEvent where
  fromAny o = WebSocketCloseEvent <$> get o "code"
                                  <*> get o "reason"
                                  <*> get o "wasClean"

-- | No-op handlers for all WebSocket events.
noHandlers :: WebSocketConfig t
noHandlers = WebSocketConfig
  { wsOpenURL   = ""
  , wsOnClose   = \_ -> return ()
  , wsOnMessage = \_ _ -> return ()
  }

-- | Open a WebSocket, returning the socket as well as a blocking CIO
--   computation to await incoming data on the socket.
--   When data arrives on the socket, the waiting computation will return
--   @Right received_data@. When the socket is closed, the waiting computation
--   will return @Left close_event@.
wsOpenSync :: WebSocketData t
           => URL
           -> CIO (Maybe (WebSocket t, CIO (Either WebSocketCloseEvent t)))
wsOpenSync url = do
  incoming <- newEmptyMVar
  msock <- wsOpen (WebSocketConfig
                   { wsOpenURL     = url
                   , wsOnClose     = putMVar incoming . Left
                   , wsOnMessage   = \_ -> putMVar incoming . Right
                   })
  case msock of
    Just sock -> return $ Just (sock, takeMVar incoming)
    _         -> return Nothing

-- | Close a WebSocket. Closing a WebSocket which has been previously closed
--   is a no-op. Any associated 'wsOnClose' handler will not be called.
wsClose :: WebSocket a -> CIO ()
wsClose ws = liftIO $ close ws



-- * Legacy API

-- | Run a computation with a WebSocket. The computation will not be executed
--   until a connection to the server has been established.
--   The WebSocket will not be closed after the computation finishes.
withWebSocket :: URL
              -- ^ URL to bind the WebSocket to
              -> (WebSocket JSString -> JSString -> CIO ())
              -- ^ Computation to run when new data arrives
              -> CIO a
              -- ^ Computation to run when an error occurs
              -> (WebSocket JSString -> CIO a)
              -- ^ Computation using the WebSocket
              -> CIO a
withWebSocket = withWS

-- | Run a computation with a WebSocket. The computation will not be executed
--   until a connection to the server has been established.
--   The WebSocket will not be closed after the computation finishes.
withBinaryWebSocket :: URL
                    -- ^ URL to bind the WebSocket to
                    -> (WebSocket Blob -> Blob -> CIO ())
                    -- ^ Computation to run when new data arrives
                    -> CIO a
                    -- ^ Computation to run when an error occurs
                    -> (WebSocket Blob -> CIO a)
                    -- ^ Computation using the WebSocket
                    -> CIO a
withBinaryWebSocket = withWS

-- | Worker for legacy API.
withWS :: WebSocketData t
       => URL
       -> (WebSocket t -> t -> CIO ())
       -> CIO a
       -> (WebSocket t -> CIO a)
       -> CIO a
withWS url cb err f = do
  mws <- wsOpen (noHandlers {wsOpenURL = url, wsOnMessage = cb})
  case mws of
    Just ws -> f ws
    _       -> err

-- | Send a Blob over a WebSocket.
wsSendBlob :: WebSocket Blob -> Blob -> CIO Bool
wsSendBlob = wsSend



-- * Hairy internals

-- | Open a new WebSocket in text mode.
new :: URL
    -> (WebSocket JSString -> JSString -> IO ())
    -> (WebSocket JSString -> IO ())
    -> (WebSocketCloseEvent -> IO ())
    -> IO ()
    -> IO ()
new = ffi "(function(url, cb, f, close, err) {\
             \var ws = new WebSocket(url);\
             \ws.onmessage = function(e) {cb(ws,e.data);};\
             \ws.onopen = function(e) {f(ws);};\
             \ws.onclose = function(e) {close(e.data));};\
             \ws.error = function(e) {err();};\
             \return ws;\
           \})" 

-- | Open a new WebSocket in binary mode.
newBin :: URL
       -> (WebSocket Blob -> Blob -> IO ())
       -> (WebSocket Blob -> IO ())
       -> (WebSocketCloseEvent -> IO ())
       -> IO ()
       -> IO ()
newBin = ffi "(function(url, cb, f, close, err) {\
                \var ws = new WebSocket(url);\
                \ws.binaryType = 'blob';\
                \ws.onmessage = function(e) {cb(ws,e.data);};\
                \ws.onopen = function(e) {f(ws);};\
                \ws.onclose = function(e) {close(e.data);};\
                \ws.onerror = function(e) {err();};\
                \return ws;\
              \})" 

-- | Send text over a WebSocket.
sendS :: WebSocket JSString -> JSString -> IO Bool
sendS = ffi "(function(s, msg) {if(s.readyState != 1) {return false;} else {s.send(msg); return true;}})"

-- | Send a blob over a WebSocket.
sendB :: WebSocket Blob -> Blob -> IO Bool
sendB = ffi "(function(s, msg) {if(s.readyState != 1) {return false;} else {s.send(msg); return true;}})"

-- | Close a WebSocket.
close :: WebSocket a -> IO ()
close = ffi "(function(ws){ws.onclose = ws.onerror = function(){}; ws.close();})"
