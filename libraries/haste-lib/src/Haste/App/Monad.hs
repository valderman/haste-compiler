{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
-- | Haste.App startup monad and configuration.
module Haste.App.Monad (
    Exportable,
    App, Server, Useless (..), Export (..), Done (..),
    AppCfg, defaultConfig, cfgURL, cfgPort,
    liftServerIO, forkServerIO, export, getAppConfig,
    mkUseful, runApp, (<.>), getSessionID
  ) where
import Control.Applicative
import Control.Monad (ap)
import Control.Monad.IO.Class
import Haste.Serialize
import Haste.JSON
import qualified Data.Map as M
import qualified Data.Set as S
import Haste.App.Protocol
import Data.Int
import Control.Concurrent (ThreadId)
#ifndef __HASTE__
import Control.Concurrent (forkIO)
import Haste.Prim (toJSStr, fromJSStr)
import Network.WebSockets as WS
import qualified Data.ByteString.Char8 as BS
import Control.Exception
import Data.IORef
import System.Random
#endif

data AppCfg = AppCfg {
    cfgURL  :: String,
    cfgPort :: Int
  }

-- | Create a default configuration from an URL and a port number.
defaultConfig :: String -> Int -> AppCfg
defaultConfig = AppCfg

type SessionID = Int64
type Sessions = S.Set SessionID
type Method = [JSON] -> SessionID -> IO JSON
type Exports = M.Map CallID Method
data Useless a = Useful a | Useless
newtype Done = Done (IO ())

data Export a = Export CallID [JSON]

-- | Apply an exported function to an argument.
--   TODO: look into making this Applicative.
(<.>) :: Serialize a => Export (a -> b) -> a -> Export b
(Export cid args) <.> arg = Export cid (toJSON arg:args)

-- | Application monad; allows for exporting functions, limited liftIO,
--   forkIO and launching the client.
newtype App a = App {
    unA :: AppCfg -> CallID -> Exports -> IO (a, CallID, Exports)
  }

instance Monad App where
  return x = App $ \_ cid exports -> return (x, cid, exports)
  (App m) >>= f = App $ \cfg cid exports -> do
    res <- m cfg cid exports
    case res of
      (x, cid', exports') -> unA (f x) cfg cid' exports'

instance Functor App where
  fmap f m = m >>= return . f

instance Applicative App where
  (<*>) = ap
  pure  = return

-- | Lift an IO action into the Server monad, the result of which can only be
--   used server-side.
liftServerIO :: IO a -> App (Useless a)
#ifdef __HASTE__
{-# RULES "throw away liftServerIO"
          forall x. liftServerIO x = return Useless #-}
liftServerIO _ = return Useless
#else
liftServerIO m = App $ \cfg cid exports -> do
  x <- m
  return (Useful x, cid, exports)
#endif

-- | Fork off an IO computation on the server. This may be useful for any tasks
--   that will keep running for as long as the server is running.
forkServerIO :: IO () -> App (Useless ThreadId)
#ifdef __HASTE__
{-# RULES "throw away forkServerIO"
          forall x. forkServerIO x = return Useless #-}
forkServerIO _ = return Useless
#else
forkServerIO m = App $ \_ cid exports -> do
  tid <- forkIO m
  return (Useful tid, cid, exports)
#endif

-- | An exportable function is of the type
--   (Serialize a, ..., Serialize result) => a -> ... -> IO result
class Exportable a where
  serializify :: a -> [JSON] -> (SessionID -> IO JSON)

instance Serialize a => Exportable (Server a) where
  serializify (Server m) _ = fmap toJSON . m

instance (Serialize a, Exportable b) => Exportable (a -> b) where
  serializify f (x:xs) = serializify (f $! fromEither $ fromJSON x) xs
    where
      fromEither (Right val) = val
      fromEither (Left e)    = error $ "Unable to deserialize data: " ++ e
  serializify _ _      = error "The impossible happened in serializify!"

-- | Make a function available to the client as an API call.
export :: Exportable a => a -> App (Export a)
#ifdef __HASTE__
{-# RULES "throw away exports"
          forall f. export f = export undefined #-}
export _ = App $ \_ cid _ ->
    return (Export cid [], cid+1, undefined)
#else
export s = App $ \_ cid exports ->
    return (Export cid [], cid+1, M.insert cid (serializify s) exports)
#endif

-- | Returns the application configuration.
getAppConfig :: App AppCfg
getAppConfig = App $ \cfg cid exports -> return (cfg, cid, exports)

-- | Run a Haste application. runApp never returns before the program
--   terminates.
runApp :: AppCfg -> App Done -> IO ()
runApp cfg (App s) = do
#ifdef __HASTE__
    (Done client, _, _) <- s cfg 0 undefined
    client
#else
    (_, _, exports) <- s cfg 0 M.empty
    serverEventLoop cfg exports
#endif

#ifndef __HASTE__
-- | Server's communication event loop. Handles dispatching API calls.
--   TODO: we could consider terminating a client who gets bad data, as that is
--         a sure side of outside interference.
serverEventLoop :: AppCfg -> Exports -> IO ()
serverEventLoop cfg exports = do
    clients <- newIORef S.empty
    WS.runServer "0.0.0.0" (cfgPort cfg) $ \pending -> do
      conn <- acceptRequest pending
      cid <- randomIO
      clientLoop cid clients conn
  where
    encode = BS.pack . fromJSStr . encodeJSON . toJSON
    
    exHandler :: SessionID -> IORef Sessions -> SomeException -> IO ()
    exHandler deadsession sessionsref _ = do
      atomicModifyIORef sessionsref $ \cs -> (S.delete deadsession cs, ())

    clientLoop :: SessionID -> IORef Sessions -> Connection -> IO ()
    clientLoop sid sref c = catch go (exHandler sid sref)
      where
        go = do
          msg <- receiveData c
          forkIO $ do
            -- Parse JSON
            case decodeJSON . toJSStr $ BS.unpack msg of
              Right json -> do
                -- Attempt to parse ServerCall from JSON and look up method
                case fromJSON json of
                  Right (ServerCall nonce method args)
                    | Just m <- M.lookup method exports -> do
                      result <- m args sid
                      sendTextData c . encode $ ServerReply {
                          srNonce = nonce,
                          srResult = result
                        }
                  _ -> do
                    error $ "Got bad method call: " ++ show json
              _ -> do
                error $ "Got bad JSON: " ++ BS.unpack msg
          go
#endif

-- | Server monad for Haste.App. Allows redeeming Useless values, lifting IO
--   actions, and not much more.
newtype Server a = Server {unS :: SessionID -> IO a}

instance Functor Server where
  fmap f (Server m) = Server $ \sid -> f <$> m sid

instance Applicative Server where
  (<*>) = ap
  pure  = return

instance Monad Server where
  return x = Server $ \_ -> return x
  (Server m) >>= f = Server $ \sid -> do
    Server m' <- f <$> m sid
    m' sid

instance MonadIO Server where
  liftIO m = Server $ \_ -> m

-- | Make a Useless value useful by extracting it. Only possible server-side,
--   in the IO monad.
mkUseful :: Useless a -> Server a
mkUseful (Useful x) = return x
mkUseful _          = error "Useless values are only useful server-side!"

-- | Returns the ID of the current session.
getSessionID :: Server SessionID
getSessionID = Server return
