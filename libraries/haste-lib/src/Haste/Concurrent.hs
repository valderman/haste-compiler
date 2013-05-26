{-# LANGUAGE GADTs #-}
-- | Implements concurrency for Haste based on "A Poor Man's Concurrency Monad".
module Haste.Concurrent (
    MVar, CIO,
    forkIO, newMVar, newEmptyMVar, takeMVar, putMVar, withMVarIO, modifyMVarIO,
    concurrent, liftIO
  ) where
import Control.Monad.IO.Class
import Data.IORef

newtype MVar a = MVar (IORef (Maybe a, [Action], [Action]))

data Action where
  Atom  :: IO Action -> Action
  Fork  :: Action -> Action -> Action
  Read  :: MVar a -> (a -> Action) -> Action
  Write :: MVar a -> a -> Action -> Action
  Stop  :: Action

-- | Concurrent IO monad. The normal IO monad does not have concurrency
--   capabilities with Haste. This monad is basically IO plus concurrency.
newtype CIO a = C {unC :: (a -> Action) -> Action}

instance Monad CIO where
  return x    = C $ \next -> next x
  (C m) >>= f = C $ \b -> m (\a -> unC (f a) b)

instance MonadIO CIO where
  liftIO m = C $ \next -> Atom (fmap next m)

-- | Spawn a new thread.
forkIO :: CIO () -> CIO ()
forkIO (C m) = C $ \next -> Fork (m (const Stop)) (next ())

-- | Create a new MVar with an initial value.
newMVar :: MonadIO m => a -> m (MVar a)
newMVar a = liftIO $ MVar `fmap` newIORef (Just a, [], [])

-- | Create a new empty MVar.
newEmptyMVar :: MonadIO m => m (MVar a)
newEmptyMVar = liftIO $ MVar `fmap` newIORef (Nothing, [], [])

-- | Read an MVar. Blocks if the MVar is empty.
takeMVar :: MVar a -> CIO a
takeMVar v = C $ \next -> Read v next

-- | Write an MVar. Blocks if the MVar is already full.
putMVar :: MVar a -> a -> CIO ()
putMVar v x = C $ \next -> Write v x (next ())

-- | Perform an IO action over an MVar.
withMVarIO :: MVar a -> (a -> IO b) -> CIO b
withMVarIO v m = takeMVar v >>= liftIO . m

-- | Perform an IO action over an MVar, then write the MVar back.
modifyMVarIO :: MVar a -> (a -> IO (a, b)) -> CIO b
modifyMVarIO v m = do
  (x, res) <- withMVarIO v m
  putMVar v x
  return res

-- | Run a concurrent computation. Two different concurrent computations may
--   share MVars; if this is the case, then a call to `concurrent` may return
--   before all the threads it spawned finish executing.
concurrent :: CIO () -> IO ()
concurrent (C m) = scheduler [m (const Stop)]
  where
    scheduler (p:ps) =
      case p of
        Atom io -> do
          next <- io
          scheduler (ps ++ [next])
        Fork a b -> do
          scheduler (ps ++ [a,b])
        -- The Read primitive is a little complicated. If there's a writer
        -- waiting for the MVar, then wake it and remove it from the wait queue.
        -- If the MVar is empty, then add the reader to the read queue and put
        -- it to sleep.
        self@(Read (MVar r) next) -> do
          (mx, readers, writers) <- readIORef r
          case mx of
            Just x -> do
              let (w, ws) = splitAt 1 writers
              writeIORef r (Nothing, readers, ws)
              scheduler (w ++ ps ++ [next x])
            _      -> do
              writeIORef r (mx, readers ++ [self], writers)
              scheduler ps
        -- The Write primitive does the exact opposite of Read.
        self@(Write (MVar r) x next) -> do
          (mx, readers, writers) <- readIORef r
          case mx of
            Nothing -> do
              let (rdr, rdrs) = splitAt 1 readers
              writeIORef r (Just x, rdrs, writers)
              scheduler (rdr ++ ps ++ [next])
            _      -> do
              writeIORef r (mx, readers, writers ++ [self])
              scheduler ps
        Stop -> do
          scheduler ps
    scheduler _ =
      return ()
