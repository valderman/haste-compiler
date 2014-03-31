{-# LANGUAGE EmptyDataDecls #-}
-- | Wraps Haste.Concurrent to work with Haste.App.
--   Task switching happens whenever a thread is blocked in an MVar, so things
--   like polling an IORef in a loop will starve all other threads.
--
--   This will likely be the state of Haste concurrency until Javascript gains
--   decent native concurrency support.
module Haste.App.Concurrent (
    C.MVar, CC.MBox, CC.Send, CC.Recv, CC.Inbox, CC.Outbox, C.MonadConc (..),
    forkMany, newMVar, newEmptyMVar, takeMVar, putMVar, peekMVar,
    CC.spawn, CC.receive, CC.statefully, (CC.!), (CC.<!),
    forever
  ) where
import qualified Haste.Concurrent.Monad as C
import qualified Haste.Concurrent as CC
import Haste.App.Client
import Control.Monad

instance C.MonadConc Client where
  liftConc = liftCIO
  fork m = do
    cs <- get id
    liftCIO . C.forkIO $ runClientCIO cs m

-- | Spawn several concurrent computations.
forkMany :: [Client ()] -> Client ()
forkMany = mapM_ C.fork

-- | Create a new MVar with the specified contents.
newMVar :: a -> Client (C.MVar a)
newMVar = liftCIO . C.newMVar

-- | Create a new empty MVar.
newEmptyMVar :: Client (C.MVar a)
newEmptyMVar = liftCIO C.newEmptyMVar

-- | Read the value of an MVar. If the MVar is empty, @takeMVar@ blocks until
--   a value arrives. @takeMVar@ empties the MVar.
takeMVar :: C.MVar a -> Client a
takeMVar = liftCIO . C.takeMVar

-- | Put a value into an MVar. If the MVar is full, @putMVar@ will block until
--   the MVar is empty.
putMVar :: C.MVar a -> a -> Client ()
putMVar v x = liftCIO $ C.putMVar v x

-- | Read an MVar without affecting its contents.
--   If the MVar is empty, @peekMVar@ immediately returns @Nothing@.
peekMVar :: C.MVar a -> Client (Maybe a)
peekMVar = liftCIO . C.peekMVar
