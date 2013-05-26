{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- | Various kinds of sinks.
module FRP.Fursuit.Sink (Sink (..), perform) where
import Data.IORef
import FRP.Fursuit.Signal
import FRP.Fursuit.Pipe

-- | Perform the IO action returned by a signal whenever triggered.
perform :: Signal (IO ()) -> IO ()
perform = sink id

-- | Bind a signal to a value of some type. Examples of instances would be
--   IORef, where ref << sig would store the value of sig in ref whenever
--   triggered, or Pipe, where p << sig would write the value of sig to p.
class Sink s a where
  (<<) :: s -> Signal a -> IO ()
infixl 0 <<

instance Sink (Pipe a) a where
  p << s = sink (write p) s

instance Sink (IORef a) a where
  ref << s = sink (writeIORef ref) s

instance Sink (a -> IO ()) a where
  act << s = sink act s
