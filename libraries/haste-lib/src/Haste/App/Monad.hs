{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
-- | Haste.App startup monad and configuration.
module Haste.App.Monad (
    Exportable,
    App, Server, Sessions, SessionID, Useless (..), Export (..), Done (..),
    AppCfg, def, mkConfig, cfgURL, cfgPort,
    liftServerIO, forkServerIO, export, getAppConfig,
    mkUseful, runApp, (<.>), getSessionID, getActiveSessions, onSessionEnd
  ) where
import Control.Applicative
import Control.Monad (ap)
import Control.Monad.IO.Class
import Haste.Binary
import Haste.Binary.Types
import qualified Data.Map as M
import qualified Data.Set as S
import Haste.App.Protocol
import Data.Word
import Control.Concurrent (ThreadId)
import Data.IORef
import Data.Default
#ifndef __HASTE__
import Haste.Binary.Types
import Control.Concurrent (forkIO)
import Haste.Prim (toJSStr, fromJSStr)
import Network.WebSockets as WS
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BU
import Control.Exception
import System.Random
import Data.List (foldl')
import Data.String
#endif

data AppCfg = AppCfg {
    cfgURL                :: String,
    cfgPort               :: Int,
    cfgSessionEndHandlers :: [SessionID -> Server ()]
  }

instance Default AppCfg where
  def = mkConfig "ws://localhost:24601" 24601

-- | Create a default configuration from an URL and a port number.
mkConfig :: String -> Int -> AppCfg
mkConfig url port = AppCfg {
    cfgURL = url,
    cfgPort = port,
    cfgSessionEndHandlers = []
  }

type SessionID = Word64
type Sessions = S.Set SessionID
type Method = [Blob] -> SessionID -> IORef Sessions -> IO Blob
type Exports = M.Map CallID Method
data Useless a = Useful a | Useless
newtype Done = Done (IO ())

data Export a = Export CallID [Blob]

-- | Apply an exported function to an argument.
--   TODO: look into making this Applicative.
(<.>) :: Binary a => Export (a -> b) -> a -> Export b
(Export cid args) <.> arg = Export cid (encode arg:args)

-- | Application monad; allows for exporting functions, limited liftIO,
--   forkIO and launching the client.
newtype App a = App {
    unA :: AppCfg
        -> IORef Sessions
        -> CallID
        -> Exports
        -> IO (a, CallID, Exports, AppCfg)
  }

instance Monad App where
  return x = App $ \c _ cid exports -> return (x, cid, exports, c)
  (App m) >>= f = App $ \cfg sessions cid exports -> do
    res <- m cfg sessions cid exports
    case res of
      (x, cid', exports', cfg') -> unA (f x) cfg' sessions cid' exports'

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
liftServerIO m = App $ \cfg _ cid exports -> do
  x <- m
  return (Useful x, cid, exports, cfg)
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
forkServerIO (Server m) = App $ \cfg sessions cid exports -> do
  tid <- forkIO $ m 0 sessions
  return (Useful tid, cid, exports, cfg)
#endif

-- | An exportable function is of the type
--   (Serialize a, ..., Serialize result) => a -> ... -> IO result
class Exportable a where
  serializify :: a -> [Blob] -> (SessionID -> IORef Sessions -> IO Blob)

instance Binary a => Exportable (Server a) where
#ifdef __HASTE__
  serializify _ _ = undefined
#else
  serializify (Server m) _ = \sid ss -> fmap encode (m sid ss)
#endif

instance (Binary a, Exportable b) => Exportable (a -> b) where
#ifdef __HASTE__
  serializify _ _ = undefined
#else
  serializify f (x:xs) = serializify (f $! fromEither $ decode (toBD x)) xs
    where
      toBD (Blob x) = BlobData x
      fromEither (Right val) = val
      fromEither (Left e)    = error $ "Unable to deserialize data: " ++ e
  serializify _ _      = error "The impossible happened in serializify!"
#endif

-- | Make a function available to the client as an API call.
export :: Exportable a => a -> App (Export a)
#ifdef __HASTE__
{-# RULES "throw away export's argument"
          forall x. export x =
            App $ \c _ cid _ -> return (Export cid [], cid+1, undefined, c) #-}
export _ = App $ \c _ cid _ ->
    return (Export cid [], cid+1, undefined, c)
#else
export s = App $ \c _ cid exports ->
    return (Export cid [], cid+1, M.insert cid (serializify s) exports, c)
#endif

-- | Register a handler to be run whenever a session terminates.
--   Several handlers can be registered at the same time; they will be run in
--   the order they were registered.
onSessionEnd :: (SessionID -> Server ()) -> App ()
#ifdef __HASTE__
{-# RULES "throw away onSessionEnd argument"
          forall x. onSessionEnd x = return () #-}
onSessionEnd _ = return ()
#else
onSessionEnd s = App $ \cfg _ cid exports -> return $
  ((), cid, exports, cfg {cfgSessionEndHandlers = s:cfgSessionEndHandlers cfg})
#endif

-- | Returns the application configuration.
getAppConfig :: App AppCfg
getAppConfig = App $ \cfg _ cid exports -> return (cfg, cid, exports, cfg)

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
    (Done client, _, _, _) <- s cfg undefined 0 undefined
    client
#else
    sessions <- newIORef S.empty
    (_, _, exports, cfg') <- s cfg sessions 0 M.empty
    serverEventLoop cfg' sessions exports
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
    cleanup :: Connection -> SessionID -> IORef Sessions -> IO ()
    cleanup conn deadsession sref = do
      let f next m = unS (m deadsession) deadsession sref >> next
      foldl' f (return ()) (cfgSessionEndHandlers cfg)
      atomicModifyIORef sref $ \cs -> (S.delete deadsession cs, ())
      let Blob bs = encode $ ServerException "Session ended"
      sendTextData conn bs

    clientLoop :: SessionID -> IORef Sessions -> Connection -> IO ()
    clientLoop sid sref c = finally go (cleanup c sid sref)
      where
        go = do
          msg <- receiveData c
          forkIO $ do
            case decode (BlobData msg) of
              Right (ServerCall nonce method args)
                | Just m <- M.lookup method exports -> do
                  result <- m args sid sref
                  let Blob bs = encode $ ServerReply {
                      srNonce = nonce,
                      srResult = result
                    }
                  sendBinaryData c bs
              Left e -> do
                error $ "Got bad method call: " ++ show msg
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

instance MonadBlob Server where
#ifndef __HASTE__
  getBlobData (Blob bd) = return $ BlobData bd
  getBlobText' (Blob bd) = return $ fromString $ BU.toString $ BS.concat $ BSL.toChunks bd
#else
  getBlobData _ = return undefined
  getBlobText' _ = return undefined
#endif

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
