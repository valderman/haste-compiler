{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
-- | Concurrency for Haste. Includes MVars, forking, Ajax and more.
module Haste.Concurrent (module Monad, module Ajax, wait, server) where
import Haste.Concurrent.Monad as Monad
import Haste.Concurrent.Ajax as Ajax
import Haste.Callback

-- | Wait for n milliseconds.
wait :: Int -> CIO ()
wait ms = do
  v <- newEmptyMVar
  liftIO $ setTimeout' ms $ putMVar v ()
  takeMVar v

-- | Creates a generic server thread. A server is a function taking a state
--   and an event argument, returning an updated state or Nothing.
--   @server@ creates an MVar that is used to pass events to the server.
--   Whenever a value is written to this MVar, that value is passed to the
--   server function togeter with its current state.
--   If the server function returns Nothing, the server thread terminates.
--   If it returns a new state, the server again blocks on the event MVar,
--   and will use the new state to any future calls to the server function.
server :: state -> (state -> evt -> CIO (Maybe state)) -> CIO (MVar evt)
server initialState handler = do
    evtvar <- newEmptyMVar
    forkIO $ loop evtvar initialState
    return evtvar
  where
    loop m st = do
      mresult <- takeMVar m >>= handler st
      case mresult of
        Just st' -> loop m st'
        _        -> return ()

instance GenericCallback (CIO ()) CIO where
  type CB (CIO ()) = IO ()
  mkcb toIO m = toIO m
  mkIOfier _ = return concurrent
