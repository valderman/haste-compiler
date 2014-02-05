{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
-- | Haste.App startup monad and configuration.
module Haste.App.Monad (
    Exportable,
    App, Server, Useless (..), Export (..), Done (..),
    AppCfg, defaultConfig, cfgURL, cfgPort,
    liftServerIO, export, mkUseful, runApp, getAppConfig, (<.>), liftIO
  ) where
import Control.Applicative
import Control.Monad (ap)
import Control.Monad.IO.Class
import Haste.Serialize
import Haste.JSON
import qualified Data.Map as M
import Haste.App.Protocol
#ifndef __HASTE__
import Control.Concurrent (forkIO)
import Haste.Prim (toJSStr, fromJSStr)
import Network.WebSockets as WS
import qualified Data.ByteString.Char8 as BS
#endif

data AppCfg = AppCfg {
    cfgURL  :: String,
    cfgPort :: Int
  }

-- | Create a default configuration from an URL and a port number.
defaultConfig :: String -> Int -> AppCfg
defaultConfig = AppCfg

type Method = [JSON] -> IO JSON
type Exports = M.Map CallID Method
data Useless a = Useful (IO a) | Useless
newtype Done = Done (IO ())

data Export a = Export CallID [JSON]

-- | Apply an exported function to an argument.
--   TODO: look into making this Applicative.
(<.>) :: Serialize a => Export (a -> b) -> a -> Export b
(Export cid args) <.> arg = Export cid (toJSON arg:args)

-- | Application monad; allows for exporting functions, limited liftIO and
--   launching the client.
newtype App a = App {
    unA :: AppCfg -> CallID -> Exports -> (a, CallID, Exports)
  }

instance Monad App where
  return x = App $ \_ cid exports -> (x, cid, exports)
  (App m) >>= f = App $ \cfg cid exports ->
    case m cfg cid exports of
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
liftServerIO _ = return Useless
#else
liftServerIO = return . Useful
#endif

-- | An exportable function is of the type
--   (Serialize a, ..., Serialize result) => a -> ... -> IO result
class Exportable a where
  serializify :: a -> [JSON] -> IO JSON

instance Serialize a => Exportable (Server a) where
  serializify (Server m) _ = fmap toJSON m

instance (Serialize a, Exportable b) => Exportable (a -> b) where
  serializify f (x:xs) = serializify (f $! fromEither $ fromJSON x) xs
    where
      fromEither (Right val) = val
      fromEither (Left e)    = error $ "Unable to deserialize data: " ++ e
  serializify _ _      = error "The impossible happened in serializify!"

-- | Make a function available to the client as an API call.
export :: Exportable a => a -> App (Export a)
export s = App $ \_ cid exports ->
    (Export cid [], cid+1, M.insert cid (serializify s) exports)

-- | Returns the application configuration.
getAppConfig :: App AppCfg
getAppConfig = App $ \cfg cid exports -> (cfg, cid, exports)

-- | Run a Haste application. runApp never returns before the program
--   terminates.
runApp :: AppCfg -> App Done -> IO ()
runApp cfg (App s) = do
#ifdef __HASTE__
    client
#else
    serverEventLoop cfg exports
#endif
  where
    (Done client, _, exports) = s cfg 0 M.empty

#ifndef __HASTE__
-- | Server's communication event loop. Handles dispatching API calls.
serverEventLoop :: AppCfg -> Exports -> IO ()
serverEventLoop cfg exports =
    WS.runServer "0.0.0.0" (cfgPort cfg) $ \pending -> do
      conn <- acceptRequest pending
      recvLoop conn
  where
    encode = BS.pack . fromJSStr . encodeJSON . toJSON
    recvLoop c = do
      msg <- receiveData c
      forkIO $ do
        -- Parse JSON
        case decodeJSON . toJSStr $ BS.unpack msg of
          Just json -> do
            -- Attempt to parse ServerCall from JSON and look up method
            case fromJSON json of
              Right (ServerCall nonce method args)
                | Just m <- M.lookup method exports -> do
                  result <- m args
                  sendTextData c . encode $ ServerReply {
                      srNonce = nonce,
                      srResult = result
                    }
              _ -> do
                error $ "Got bad method call: " ++ show json
          _ -> do
            error $ "Got bad JSON: " ++ BS.unpack msg
      recvLoop c
#endif

-- | Server monad for Haste.App. Allows redeeming Useless values, lifting IO
--   actions, and not much more.
newtype Server a = Server (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Make a Useless value useful by extracting it. Only possible server-side,
--   in the IO monad.
mkUseful :: Useless a -> Server a
mkUseful (Useful m) = liftIO m
mkUseful _          = error "Useless values are only useful server-side!"
