{-# LANGUAGE CPP #-}
-- | XHaste Server monad.
module XHaste.Server (
    Exportable,
    Server, Useless, Export (..), Done (..),
    liftIO, export, mkUseful, runServer, (<.>)
  ) where
import Control.Applicative
import Control.Monad (ap)
import Haste.Serialize
import Haste.JSON
import qualified Data.Map as M
import XHaste.Protocol

type Method = [JSON] -> IO JSON
type Exports = M.Map CallID Method
data Useless a = Useful (IO a) | Useless
newtype Done = Done (IO ())

data Export a = Export CallID [JSON]

-- | Apply an exported function to an argument.
--   TODO: look into making this Applicative.
(<.>) :: Serialize a => Export (a -> b) -> a -> Export b
(Export cid args) <.> arg = Export cid (toJSON arg:args)

-- | Make a Useless value useful by extracting it. Only possible server-side,
--   in the IO monad.
mkUseful :: Useless a -> IO a
mkUseful (Useful m) = m
mkUseful _          = error "Useless values are only useful server-side!"

-- | Server monad; allows for exporting functions, limited liftIO and
--   launching the client.
newtype Server a = Server {
    unS :: CallID -> Exports -> (a, CallID, Exports)
  }

instance Monad Server where
  return x = Server $ \cid exports -> (x, cid, exports)
  (Server m) >>= f = Server $ \cid exports ->
    case m cid exports of
      (x, cid', exports') -> unS (f x) cid' exports'

instance Functor Server where
  fmap f m = m >>= return . f

instance Applicative Server where
  (<*>) = ap
  pure  = return

-- | Lift an IO action into the Server monad, the result of which can only be
--   used server-side.
liftIO :: IO a -> Server (Useless a)
#ifdef __HASTE__
liftIO _ = return Useless
#else
liftIO = return . Useful
#endif

-- | An exportable function is of the type
--   (Serialize a, ..., Serialize result) => a -> ... -> IO result
class Exportable a where
  serializify :: a -> [JSON] -> IO JSON

instance Serialize a => Exportable (IO a) where
  serializify m _ = fmap toJSON m

instance (Serialize a, Exportable b) => Exportable (a -> b) where
  serializify f (x:xs) = serializify (f $! fromEither $ fromJSON x) xs
    where
      fromEither (Right x) = x
      fromEither (Left e)  = error $ "Unable to deserialize data: " ++ e

-- | Make a function available to the client as an API call.
export :: Exportable a => a -> Server (Export a)
export s = Server $ \cid exports ->
    (Export cid [], cid+1, M.insert cid (serializify s) exports)

-- | Run a server computation. runServer never returns before the program
--   terminates.
runServer :: Server Done -> IO ()
runServer (Server s) = do
#ifdef __HASTE__
    client
#else
    serverEventLoop exports
#endif
  where
    (Done client, _, exports) = s 0 M.empty

-- | Server's communication event loop. Handles dispatching API calls.
serverEventLoop :: Exports -> IO ()
serverEventLoop exports = error "Not implemented!"

-- TODO:
-- * call/onServer should take care of data transmission as well. For instance,
--   call Export (Bool -> IO Bool) -> Server (Bool -> IO Bool)
--   call f = return $ \x -> send x >> waitForReturn
--   If we have Export cid :: Export (Int -> Bool -> IO Int) coming in, we want
--   \x y -> __call (Export cid) [toJSON x, toJSON y] out
-- * runServer should go into its event loop after running s
