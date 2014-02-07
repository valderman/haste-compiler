-- | Wraps Haste.Concurrent to work with Haste.App.
--   Task switching happens whenever a thread is blocked in an MVar, so things
--   like polling an IORef in a loop will starve all other threads.
--
--   This will likely be the state of Haste concurrency until Javascript gains
--   decent native concurrency support.
module Haste.App.Concurrent (
    C.MVar,
    fork, forkMany, newMVar, newEmptyMVar, takeMVar, putMVar, peekMVar
  ) where
import qualified Haste.Concurrent.Monad as C
import Haste.App.Client

-- | Spawn off a concurrent computation.
fork :: Client () -> Client ()
fork m = do
  cs <- get id
  liftCIO . C.forkIO $ runClientCIO cs m

-- | Spawn several concurrent computations.
forkMany :: [Client ()] -> Client ()
forkMany = mapM_ fork

newMVar :: a -> Client (C.MVar a)
newMVar = liftCIO . C.newMVar

newEmptyMVar :: Client (C.MVar a)
newEmptyMVar = liftCIO C.newEmptyMVar

takeMVar :: C.MVar a -> Client a
takeMVar = liftCIO . C.takeMVar

putMVar :: C.MVar a -> a -> Client ()
putMVar v x = liftCIO $ C.putMVar v x

-- | Read an MVar without affecting its contents.
--   If the MVar is empty, @peekMVar@ immediately returns @Nothing@.
peekMVar :: C.MVar a -> Client (Maybe a)
peekMVar = liftCIO . C.peekMVar
