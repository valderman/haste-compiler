{-# LANGUAGE GADTs, FlexibleInstances, UndecidableInstances #-}
module Haste.Reactive.Signal (
    Signal, Source, buffered, lazy, source, push, perform, start, sink, evaluate
  ) where
import Data.IORef
import Control.Applicative
import Control.Monad
import System.IO.Unsafe

data AnySignal where
  AnySignal :: SigLike a => a -> AnySignal

class SigLike a where
  -- | Activate the signal, notifying all listeners.
  activate     :: a -> IO ()
  -- | Add b as a listener of a.
  listensTo    :: AnySignal -> a -> IO ()
  -- | Get all signals listening to this signal.
  getListeners :: a -> IO [AnySignal]

newtype Source a = Source (Signal a)

-- | Represents a signal for listener dependency purposes. The Self value is
--   special in that it represents the final version of the Signal being built.
--   If we didn't do this, foo :: Signal a -> Signal b -> Signal c would
--   generate dependencies for the indermediate Signal a -> Signal (b -> c) 
--   values too!
--   We must, however, convert every instance of Self into Other whenever a
--   signal is used as an argument to another!
data Listener = Self | Other AnySignal

data Signal a = Signal {
    -- | The action that produces the signal's output.
    trigger      :: IO a,
    -- | A function that, given the previous and the new output values of the
    --   signal determines whether to trigger the signal's listeners or not.
    propagate    :: a -> a -> Bool,
    -- | The signal's output is written here, so it can be read by other
    --   signals.
    output       :: IORef a,
    -- | All signals currently listening to this signal.
    listeners    :: IORef [AnySignal],
    -- | All signal listener relationships needed for this signal to function.
    dependencies :: [(Listener, AnySignal)]
  }

instance SigLike AnySignal where
  activate  (AnySignal s) =
    activate s
  listensTo listener (AnySignal speaker) =
    listener `listensTo` speaker
  getListeners (AnySignal s) =
    getListeners s

instance SigLike (Signal a) where
  activate sig = do
    old <- readIORef out
    new <- trigger sig
    writeIORef out new
    when (propagate sig old new) $ do
      ls <- readIORef (listeners sig)
      mapM_ activate ls
    where
      out = output sig
  
  listensTo l sig = modifyIORef (listeners sig) $ \ls -> l : ls
  getListeners    = readIORef . listeners

-- | Evaluate a signal, and save and return its output, but don't notify any
--   listeners.
evaluate :: Signal a -> IO a
evaluate sig = do
  res <- trigger sig
  writeIORef (output sig) res
  return res

unSelf :: Signal a -> (Listener, AnySignal) -> (Listener, AnySignal)
unSelf sig (Self, dep) = (Other $ AnySignal sig, dep)
unSelf _ x             = x

instance Functor Signal where
  fmap f sig = Signal {
      trigger       = trigger sig >>= return . f,
      output        = newRef sig undefined,
      listeners     = newRef sig [],
      propagate     = alwaysPropagate,
      dependencies = (Self, AnySignal sig) : map (unSelf sig) (dependencies sig)
    }

-- | Applicative is the main interface for composing signals. Note that the
--   <*> operation will reset the propagation control function of its left hand
--   operand; thus, lazy foo <*> bar <==> foo <*> bar.
instance Applicative Signal where
  pure x = Signal {
      trigger      = pure x,
      propagate    = alwaysPropagate,
      output       = newRef x x,
      listeners    = newRef x [],
      dependencies = []
    }

  sig <*> x = Signal {
      trigger      = app,
      output       = newRefIO x app,
      listeners    = newRef x [],
      propagate    = alwaysPropagate,
      dependencies =
        (Self, AnySignal x) : dependencies sig ++ map (unSelf x) (dependencies x)
    }
    where
      app = do
        x' <- readIORef (output x)
        f <- evaluate sig
        return $ f x'

-- | Set upp all connections for a given signal and make it respond to inputs.
--   An IORef always pointing to the latest output from the signal is returned.
start :: Signal a -> IO (IORef a)
start sig = do
  sequence_ $ map addDep (dependencies sig)
  return $ output sig
  where
    addDep (Self, dep)    = AnySignal sig `listensTo` dep
    addDep (Other s, dep) = s `listensTo` dep

-- | Attach a "sink" to the end of a signal. The sink is an IO action that gets
--   executed whenever the signal is triggered.
sink :: (a -> IO ()) -> Signal a-> IO ()
sink act sig = do
  _ <- start $ perform $ act <$> sig
  return ()

-- | Turn a signal that would output an IO computation into one which will
--   perform the computation and output its result when activated.
--   Before the signal has been activated once, its output is undefined.
perform :: Signal (IO a) -> Signal a
perform sig = self
  where
    self = Signal {trigger      = join $ trigger sig,
                   output       = newRefIO sig (return undefined),
                   listeners    = listeners sig,
                   propagate    = neverPropagate,
                   dependencies = dependencies sig}

-- | A buffered signal never activates its listeners.
buffered :: Signal a -> Signal a
buffered sig = self
  where
    self = Signal {trigger      = readIORef (output sig),
                   output       = newRefIO sig (readIORef $ output sig),
                   listeners    = newRef sig [],
                   propagate    = neverPropagate,
                   dependencies =
                     (Other$AnySignal self, AnySignal sig) : map (unSelf sig) (dependencies sig)}

-- | A lazy signal only activates its listeners whenever its output changes.
lazy :: (Show a, Eq a) => Signal a -> Signal a
lazy sig = initial `seq` self
  where
    act = readIORef $ output sig
    initial = newRefIO sig $ evaluate sig
    self = Signal {trigger      = act,
                   output       = initial,
                   listeners    = newRef sig [],
                   propagate    = (/=),
                   dependencies =
                     (Other$AnySignal self, AnySignal sig) : map (unSelf sig) (dependencies sig)}

-- | Create a signal source. A source lets you raise signals by 'push'ing
--   values into it. Each pushed value will trigger the corresponding signal.
source :: a -> IO (Source a, Signal a)
source initial = do
  out <- newIORef initial
  ls <- newIORef []
  let self = Signal {
          trigger      = readIORef out,
          output       = out,
          listeners    = ls,
          propagate    = alwaysPropagate,
          dependencies = []
        }
  return (Source self, self)

-- | Push a value into an event source.
push :: a -> Source a -> IO ()
push x (Source sig) = do
  writeIORef (output sig) x
  activate sig

{-# NOINLINE newRef #-}
newRef :: a -> b -> IORef b
newRef _ val = unsafePerformIO $ newIORef val

{-# NOINLINE newRefIO #-}
newRefIO :: a -> IO b -> IORef b
newRefIO _ val = unsafePerformIO $! val >>= newIORef

alwaysPropagate :: a -> a -> Bool
alwaysPropagate a b = True

neverPropagate :: a -> a -> Bool
neverPropagate a b = False
