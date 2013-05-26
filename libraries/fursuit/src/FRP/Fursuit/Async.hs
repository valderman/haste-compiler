module FRP.Fursuit.Async where
import Control.Applicative
import FRP.Fursuit.Signal
import FRP.Fursuit.Sink
import FRP.Fursuit.Pipe

-- | Create an asynchronous signal. When the signal is triggered, it should
--   arrange for a value to written into its obtained pipe at some point in
--   the future; when that happens, the signal continues on its path.
--
--   The signal setup action is guaranteed to be called exactly once per
--   received signal; thus the following code will print 1 two times rather
--   than 1 followed by 2:
-- @
--   ref <- newIORef ()
--   (p, s) <- pipe ()
--   sig <- async $ (\p -> modifyIORef ref (+1) >> write p ref) <$ s
--   sink (\r -> readIORef r >>= print) sig
--   sink (\r -> readIORef r >>= print) sig
--   write p ()
-- @
--   Async is primarily intended as a building block for other more high level
--   signals, and should be used with extreme care as it permits arbitrary side
--   effects to happen in the course of propagating a signal.
--
--   It's also worth noting that triggering the signal returned by async does
--   not trigger the setup signal, but will instead use the last value held by
--   the output signal, if any.
async :: Signal (Pipe a -> IO ()) -> IO (Signal a)
async setup = do
  (p,s) <- emptyPipe
  perform $ setup <*> pure p
  return s
