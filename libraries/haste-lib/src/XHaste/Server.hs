{-# LANGUAGE CPP #-}
-- | XHaste Server monad.
module XHaste.Server (
    Exportable,
    Server, Useless, Export (..),
    liftIO, export, mkUseful, unS
  ) where
import Control.Applicative
import Control.Monad (ap)
import Haste.Serialize
import Haste.JSON
import qualified Data.Map as M
import Unsafe.Coerce

type Nonce = Int
type CallID = Int
type Method = [JSON] -> IO JSON
type Exports = M.Map CallID Method
newtype Export a = Export CallID
data Useless a = Useful (IO a) | Useless

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
    (Export cid, cid+1, M.insert cid (serializify s) exports)

-- TODO:
-- * call/onServer should take care of data transmission as well. For instance,
--   call Export (Bool -> IO Bool) -> Server (Bool -> IO Bool)
--   call f = return $ \x -> send x >> waitForReturn
