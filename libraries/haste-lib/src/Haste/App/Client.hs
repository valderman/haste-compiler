{-# LANGUAGE OverloadedStrings, TypeFamilies, MultiParamTypeClasses,
             FlexibleInstances #-}
module Haste.App.Client (
    Client, ClientState,
    runClient, onServer, liftCIO, get, runClientCIO
  ) where
import Haste
import Haste.WebSockets
import Haste.Binary hiding (get)
import Haste.App.Monad
import Haste.App.Protocol
import Control.Applicative
import Control.Monad (ap, join)
import Control.Monad.IO.Class
import Control.Exception (throw)
import Data.IORef

data ClientState = ClientState {
    csWebSocket  :: WebSocket,
    csNonce      :: IORef Int,
    csResultVars :: IORef [(Int, MVar Blob)]
  }

initialState :: IORef Int -> IORef [(Int,MVar Blob)] -> WebSocket -> ClientState
initialState n mv ws =
  ClientState {
    csWebSocket  = ws,
    csNonce      = n,
    csResultVars = mv
  }

-- | A client-side computation. See it as XHaste's version of the IO monad.
newtype Client a = Client {
    unC :: ClientState -> CIO a
  }

instance Monad Client where
  (Client m) >>= f = Client $ \cs -> do
    x <- m cs
    unC (f x) cs
  return x = Client $ \_ -> return x

instance Functor Client where
  fmap f (Client m) = Client $ \cs -> fmap f (m cs)

instance Applicative Client where
  (<*>) = ap
  pure  = return

instance MonadIO Client where
  liftIO m = Client $ \_ -> do
    x <- liftIO m
    return x

instance GenericCallback (Client ()) Client where
  type CB (Client ()) = IO ()
  mkcb toIO m = toIO m
  mkIOfier _ = do
    st <- get id
    return $ concurrent . runClientCIO st

instance MonadBlob Client where
  getBlobData = liftCIO . getBlobData
  getBlobText' = liftCIO . getBlobText'


-- | Lift a CIO action into the Client monad.
liftCIO :: CIO a -> Client a
liftCIO m = Client $ \_ -> m >>= \x -> return x

-- | Get part of the client state.
get :: (ClientState -> a) -> Client a
get f = Client $ \cs -> return (f cs)

-- | Create a new nonce with associated result var.
newResult :: Client (Int, MVar Blob)
newResult = Client $ \cs -> do
  mv <- newEmptyMVar
  nonce <- liftIO $ atomicModifyIORef (csNonce cs) $ \n -> (n+1, n)
  liftIO $ atomicModifyIORef (csResultVars cs) $ \vs -> ((nonce, mv):vs, ())
  return (nonce, mv)

-- | Run a Client computation in the web browser. The URL argument specifies
--   the WebSockets URL the client should use to find the server.
runClient_ :: URL -> Client () -> IO ()
runClient_ url (Client m) = concurrent $ do
    mv <- liftIO $ newIORef []
    n <- liftIO $ newIORef 0
    let errorhandler = error "WebSockets connection died for some reason!"
        computation ws = m (initialState n mv ws)
    withBinaryWebSocket url (handler mv) errorhandler computation
  where
    -- When a message comes in, attempt to extract from it two members "nonce"
    -- and "result". Find the result MVar corresponding to the nonce and write
    -- the result to it, then discard the MVar.
    -- TODO: if the nonce is not found, the result vars list if left as it was.
    --       Maybe we should crash here instead, since this is clearly an
    --       unrecoverable error?
    handler rvars _ msg = do
      msg' <- getBlobData msg
      join . liftIO $ atomicModifyIORef rvars $ \vs ->
        let res = do
              case decode msg' :: Either String ServerException of
                Right e -> throw e
                _       -> return ()
              ServerReply nonce result <- decode msg'
              (var, vs') <- case span (\(n, _) -> n /= nonce) vs of
                              (xs, ((_, y):ys)) -> Right (y, xs ++ ys)
                              _                 -> Left "Bad nonce!"
              return (var, result, vs')
        in case res of
             Right (resvar, result, vs') -> (vs', putMVar resvar result)
             _                           -> (vs, return ())

-- | Launch a client from a Server computation. runClient never returns before
--   the program terminates.
runClient :: Client () -> App Done
runClient m = do
  url <- cfgURL `fmap` getAppConfig
  return . Done $ runClient_ url m

-- | Run a client computation from the CIO monad, using a pre-specified state.
runClientCIO :: ClientState -> Client a -> CIO a
runClientCIO cs (Client m) = m cs

-- | Perform a server-side computation, blocking the client thread until said
--   computation returns. All free variables in the server-side computation
--   which originate in the Client monad must be serializable.
onServer :: Binary a => Export (Server a) -> Client a
onServer (Export cid args) = __call cid (reverse args)

-- | Make a server-side call.
__call :: Binary a => CallID -> [Blob] -> Client a
__call cid args = do
  ws <- get csWebSocket
  (nonce, mv) <- newResult
  liftCIO . wsSendBlob ws . encode $ ServerCall {
      scNonce = nonce,
      scMethod = cid,
      scArgs = args
    }
  resblob <- liftCIO $ takeMVar mv
  res <- getBlobData resblob
  case decode res of
    Right x -> return x
    Left _  -> fail $ "Unable to decode return value!"
