-- | Wraps Haste.Concurrent to work with Haste.App.
--   Task switching happens whenever a thread is blocked in an MVar, so things
--   like polling an IORef in a loop will starve all other threads.
--
--   This will likely be the state of Haste concurrency until Javascript gains
--   decent native concurrency support.
module Haste.App.Concurrent (
    C.MVar,
    fork, forkMany, newMVar, newEmptyMVar, takeMVar, putMVar, peekMVar,
    server
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

-- | Creates a generic server thread. A server is a function taking a state
--   and an event argument, returning an updated state or Nothing.
--   @server@ creates an MVar that is used to pass events to the server.
--   Whenever a value is written to this MVar, that value is passed to the
--   server function togeter with its current state.
--   If the server function returns Nothing, the server thread terminates.
--   If it returns a new state, the server again blocks on the event MVar,
--   and will use the new state to any future calls to the server function.
server :: state -> (state -> evt -> Client (Maybe state)) -> Client (C.MVar evt)
server initialState handler = do
    evtvar <- newEmptyMVar
    fork $ loop evtvar initialState
    return evtvar
  where
    loop m st = do
      mresult <- takeMVar m >>= handler st
      case mresult of
        Just st' -> loop m st'
        _        -> return ()
