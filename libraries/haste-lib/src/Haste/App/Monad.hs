{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
-- | Haste.App startup monad and configuration.
module Haste.App.Monad (
    Exportable,
    App, Server, Sessions, SessionID, Useless (..), Export (..), Done (..),
    AppCfg, defaultConfig, cfgURL, cfgPort,
    liftServerIO, forkServerIO, export, getAppConfig,
    mkUseful, runApp, (<.>), getSessionID, getActiveSessions
  ) where
import Control.Applicative
import Control.Monad (ap)
import Control.Monad.IO.Class
import Haste.Serialize
import Haste.JSON
import qualified Data.Map as M
import qualified Data.Set as S
import Haste.App.Protocol
import Data.Word
import Control.Concurrent (ThreadId)
import Data.IORef
#ifndef __HASTE__
import Control.Concurrent (forkIO)
import Haste.Prim (toJSStr, fromJSStr)
import Network.WebSockets as WS
import qualified Data.ByteString.Char8 as BS
import Control.Exception
import System.Random
#endif

data AppCfg = AppCfg {
    cfgURL  :: String,
    cfgPort :: Int
  }

-- | Create a default configuration from an URL and a port number.
defaultConfig :: String -> Int -> AppCfg
defaultConfig = AppCfg

type SessionID = Word64
type Sessions = S.Set SessionID
type Method = [JSON] -> SessionID -> IORef Sessions -> IO JSON
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
    unA :: AppCfg
        -> IORef Sessions
        -> CallID
        -> Exports
        -> IO (a, CallID, Exports)
  }

instance Monad App where
  return x = App $ \_ _ cid exports -> return (x, cid, exports)
  (App m) >>= f = App $ \cfg sessions cid exports -> do
    res <- m cfg sessions cid exports
    case res of
      (x, cid', exports') -> unA (f x) cfg sessions cid' exports'

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
liftServerIO m = App $ \_ _ cid exports -> do
  x <- m
  return (Useful x, cid, exports)
#endif

-- | Fork off a Server computation not bound an API call.
--   This may be useful for any tasks that will keep running for as long as
--   the server is running.
--
--   Calling @getSessionID@ inside this computation will return 0, which will
--   never be generated for an actual session. @getActiveSessions@ works as
--   expected.
forkServerIO :: Server () -> App (Useless ThreadId)
#ifdef __HASTE__
{-# RULES "throw away forkServerIO"
          forall x. forkServerIO x = return Useless #-}
forkServerIO _ = return Useless
#else
forkServerIO (Server m) = App $ \_ sessions cid exports -> do
  tid <- forkIO $ m 0 sessions
  return (Useful tid, cid, exports)
#endif

-- | An exportable function is of the type
--   (Serialize a, ..., Serialize result) => a -> ... -> IO result
class Exportable a where
  serializify :: a -> [JSON] -> (SessionID -> IORef Sessions -> IO JSON)

instance Serialize a => Exportable (Server a) where
  serializify (Server m) _ = \sid ss -> fmap toJSON (m sid ss)

instance (Serialize a, Exportable b) => Exportable (a -> b) where
  serializify f (x:xs) = serializify (f $! fromEither $ fromJSON x) xs
    where
      fromEither (Right val) = val
      fromEither (Left e)    = error $ "Unable to deserialize data: " ++ e
  serializify _ _      = error "The impossible happened in serializify!"

-- | Make a function available to the client as an API call.
export :: Exportable a => a -> App (Export a)
#ifdef __HASTE__
{-# RULES "throw away export's argument"
          forall x. export x =
            App $ \_ _ cid _ -> return (Export cid [], cid+1, undefined) #-}
export _ = App $ \_ _ cid _ ->
    return (Export cid [], cid+1, undefined)
#else
export s = App $ \_ _ cid exports ->
    return (Export cid [], cid+1, M.insert cid (serializify s) exports)
#endif

-- | Returns the application configuration.
getAppConfig :: App AppCfg
getAppConfig = App $ \cfg _ cid exports -> return (cfg, cid, exports)

-- | Run a Haste.App application. runApp never returns before the program
--   terminates.
--
--   Note that @runApp@'s arguments *must not* depend on any external IO,
--   or the client and server computations may diverge.
--   Ideally, calling runApp should be the first and only thing that happens
--   in main.
runApp :: AppCfg -> App Done -> IO ()
runApp cfg (App s) = do
#ifdef __HASTE__
    (Done client, _, _) <- s cfg undefined 0 undefined
    client
#else
    sessions <- newIORef S.empty
    (_, _, exports) <- s cfg sessions 0 M.empty
    serverEventLoop cfg sessions exports
#endif

#ifndef __HASTE__
-- | Server's communication event loop. Handles dispatching API calls.
--   TODO: we could consider terminating a client who gets bad data, as that is
--         a sure side of outside interference.
serverEventLoop :: AppCfg -> IORef Sessions -> Exports -> IO ()
serverEventLoop cfg sessions exports = do
    WS.runServer "0.0.0.0" (cfgPort cfg) $ \pending -> do
      conn <- acceptRequest pending
      sid <- randomRIO (1, 0xFFFFFFFFFFFFFFFF)
      atomicModifyIORef sessions $ \s -> (S.insert sid s, ())
      clientLoop sid sessions conn
  where
    encode = BS.pack . fromJSStr . encodeJSON . toJSON
    
    cleanup :: SessionID -> IORef Sessions -> IO ()
    cleanup deadsession sessionsref = do
      atomicModifyIORef sessionsref $ \cs -> (S.delete deadsession cs, ())

    clientLoop :: SessionID -> IORef Sessions -> Connection -> IO ()
    clientLoop sid sref c = finally go (cleanup sid sref)
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
                      result <- m args sid sref
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
newtype Server a = Server {unS :: SessionID -> IORef Sessions -> IO a}

instance Functor Server where
  fmap f (Server m) = Server $ \sid ss -> f <$> m sid ss

instance Applicative Server where
  (<*>) = ap
  pure  = return

instance Monad Server where
  return x = Server $ \_ _ -> return x
  (Server m) >>= f = Server $ \sid ss -> do
    Server m' <- f <$> m sid ss
    m' sid ss

instance MonadIO Server where
  liftIO m = Server $ \_ _ -> m

-- | Make a Useless value useful by extracting it. Only possible server-side,
--   in the IO monad.
mkUseful :: Useless a -> Server a
mkUseful (Useful x) = return x
mkUseful _          = error "Useless values are only useful server-side!"

-- | Returns the ID of the current session.
getSessionID :: Server SessionID
getSessionID = Server $ \sid _ -> return sid

-- | Return all currently active sessions.
getActiveSessions :: Server Sessions
getActiveSessions = Server $ \_ ss -> readIORef ss
