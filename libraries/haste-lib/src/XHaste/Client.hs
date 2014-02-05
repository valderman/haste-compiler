{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module XHaste.Client (
    Client, runClient, onServer, liftCIO
  ) where
import Haste
import Haste.Serialize
import Haste.WebSockets hiding (Method)
import Haste.JSON
import Haste.Concurrent hiding (Method)
import XHaste.Server
import XHaste.Protocol
import System.IO.Unsafe

data ClientState = ClientState {
    csWebSocket  :: WebSocket,
    csNonce      :: Int,
    csResultVars :: MVar [(Int, MVar JSON)]
  }

initialState :: MVar [(Int, MVar JSON)] -> WebSocket -> ClientState
initialState mv ws =
  ClientState {
    csWebSocket  = ws,
    csNonce      = 0,
    csResultVars = mv
  }

-- | A client-side computation. See it as XHaste's version of the IO monad.
newtype Client a = Client {
    unC :: ClientState -> CIO (ClientState, a)
  }

instance Monad Client where
  (Client m) >>= f = Client $ \cs -> do
    (cs', x) <- m cs
    unC (f x) cs'
  return x = Client $ \cs -> return (cs, x)

-- | Lift a CIO action into the Client monad.
liftCIO :: CIO a -> Client a
liftCIO m = Client $ \cs -> m >>= \x -> return (cs, x)

-- | Get part of the client state.
get :: (ClientState -> a) -> Client a
get f = Client $ \cs -> return (cs, f cs)

-- | Create a new nonce with associated result var.
newResult :: Client (Int, MVar JSON)
newResult = Client $ \cs -> do
  mv <- newEmptyMVar
  let nonce = csNonce cs
      varsvar = csResultVars cs
  vars <- takeMVar varsvar
  putMVar varsvar $ (nonce, mv) : vars
  return (cs {csNonce = nonce + 1}, (nonce, mv))

-- | Run a Client computation in the web browser. The URL argument specifies
--   the WebSockets URL the client should use to find the server.
runClient_ :: URL -> Client () -> IO ()
runClient_ url (Client m) = concurrent $ do
    mv <- newMVar []
    let errorhandler = error "WebSockets connection died for some reason!"
        computation ws = snd `fmap` m (initialState mv ws)
    withWebSocket url (handler mv) errorhandler computation
  where
    -- When a message comes in, attempt to extract from it two members "nonce"
    -- and "result". Find the result MVar corresponding to the nonce and write
    -- the result to it, then discard the MVar.
    handler rvars ws msg = do
      vs <- takeMVar rvars
      let res = do
            ServerReply nonce result <- liftMaybe (decodeJSON msg) >>= fromJSON
            (var, vs') <- case span (\(n, _) -> n /= nonce) vs of
                            (xs, ((_, y):ys)) -> Right (y, xs ++ ys)
                            _                 -> Left "Wrong nonce"
            return (var, result, vs')
      case res of
        Right (resvar, result, vs') -> do
          putMVar resvar result
          putMVar rvars vs'
        _ -> do
          putMVar rvars vs

-- | Launch a client from a Server computation. runClient never returns before
--   the program terminates.
runClient :: Client () -> App Done
runClient m = do
  url <- cfgURL `fmap` getAppConfig
  return . Done $ runClient_ url m

-- | Perform a server-side computation, blocking the client thread until said
--   computation returns. All free variables in the server-side computation
--   which originate in the Client monad must be serializable.
onServer :: Serialize a => Export (IO a) -> Client a
onServer (Export cid args) = __call cid (reverse args)

-- | Make a server-side call.
__call :: Serialize a => CallID -> [JSON] -> Client a
__call cid args = do
  ws <- get csWebSocket
  (nonce, mv) <- newResult
  liftCIO . wsSend ws . encodeJSON . toJSON $ ServerCall {
      scNonce = nonce,
      scMethod = cid,
      scArgs = args
    }
  res <- liftCIO (takeMVar mv)
  case fromJSON res of
    Right x -> return x
    Left e  -> fail $ "Unable to decode JSON: " ++ show res
