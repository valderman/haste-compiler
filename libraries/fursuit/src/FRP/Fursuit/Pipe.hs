{-# LANGUAGE BangPatterns, CPP #-}
-- | Pipes are the only way to cause a 'Signal' to fire from the outside.
module FRP.Fursuit.Pipe (pipe, emptyPipe, write) where
import Data.IORef
import FRP.Fursuit.Signal
import qualified Data.IntMap as M
import FRP.Fursuit.Locking

-- | Create a pipe. Writing to a pipe is the only way to manually trigger a
--   signal.
emptyPipe :: IO (Pipe a, Signal a)
emptyPipe = do
  ref <- newIORef Nothing
  orig <- newIORef False
  sinks <- newIORef M.empty
  return (P ref sinks orig, Pipe ref sinks orig)

-- | Create a pipe with an initial value.
pipe :: a -> IO (Pipe a, Signal a)
pipe !initially = do
  ps@(P ref _ _, _) <- emptyPipe
  writeIORef ref (Just initially)
  return ps

-- | Write a value into a pipe. This will cause the pipe's associated signal
--   to fire.
write :: Pipe a -> a -> IO ()
write (P value listeners origin) !x = withFursuitLock $ do
  writeIORef origin True
  writeIORef value (Just x)
  readIORef listeners >>= M.foldl' (>>) (return ())
  writeIORef origin False
