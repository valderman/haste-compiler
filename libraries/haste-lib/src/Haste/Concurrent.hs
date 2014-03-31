{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies,
             EmptyDataDecls #-}
-- | Concurrency for Haste. Includes MVars, forking, Ajax and more.
module Haste.Concurrent (
    module Monad, module Ajax,
    Recv, Send, Inbox, Outbox, MBox,
    receive, spawn, statefully, (!), (<!),
    wait
  ) where
import Haste.Concurrent.Monad as Monad
import Haste.Concurrent.Ajax as Ajax hiding ((!))
import Haste.Callback

-- | Wait for n milliseconds.
wait :: Int -> CIO ()
wait ms = do
  v <- newEmptyMVar
  liftIO $ setTimeout' ms $ putMVar v ()
  takeMVar v

instance GenericCallback (CIO ()) CIO where
  type CB (CIO ()) = IO ()
  mkcb toIO m = toIO m
  mkIOfier _ = return concurrent

-- | An MBox is a read/write-only MVar, depending on its first type parameter.
--   Used to communicate with server processes.
newtype MBox t a = MBox (MVar a)

data Recv
data Send

type Inbox = MBox Recv
type Outbox = MBox Send

-- | Block until a message arrives in a mailbox, then return it.
receive :: MonadConc m => Inbox a -> m a
receive (MBox mv) = liftConc $ takeMVar mv

-- | Creates a generic process and returns a MBox which may be used to pass
--   messages to it.
--   While it is possible for a process created using spawn to transmit its
--   inbox to someone else, this is a very bad idea; don't do it.
spawn :: MonadConc m => (Inbox a -> m ()) -> m (Outbox a)
spawn f = do
  p <- liftConc newEmptyMVar
  fork $ f (MBox p)
  return (MBox p)

-- | Creates a generic stateful process. This process is a function taking a
--   state and an event argument, returning an updated state or Nothing.
--   @statefully@ creates a @MBox@ that is used to pass events to the process.
--   Whenever a value is written to this MBox, that value is passed to the
--   process function together with the function's current state.
--   If the process function returns Nothing, the process terminates.
--   If it returns a new state, the process again blocks on the event MBox,
--   and will use the new state to any future calls to the server function.
statefully :: MonadConc m => st -> (st -> evt -> m (Maybe st)) -> m (Outbox evt)
statefully initialState handler = do
    spawn $ loop initialState
  where
    loop st p = do
      mresult <- liftConc (receive p) >>= handler st
      case mresult of
        Just st' -> loop st' p
        _        -> return ()

-- | Write a value to a MBox. Named after the Erlang message sending operator,
--   as both are intended for passing messages to processes.
--   This operation does not block until the message is delivered, but returns
--   immediately.
(!) :: MonadConc m => Outbox a -> a -> m ()
MBox m ! x = liftConc $ forkIO $ putMVar m x

-- | Perform a Client computation, then write its return value to the given
--   pipe. Mnemonic: the operator is a combination of <- and !.
--   Just like @(!)@, this operation is non-blocking.
(<!) :: MonadConc m => Outbox a -> m a -> m ()
p <! m = do x <- m ; p ! x
