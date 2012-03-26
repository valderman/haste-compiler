{-# LANGUAGE GADTs, FlexibleInstances, UndecidableInstances #-}
module Haste.Reactive.Signal (
    Signal, Source, buffered, lazy, source, push, perform, start, sink, evaluate
  ) where
import Data.IORef
import Control.Applicative
import Control.Monad
import System.IO.Unsafe
import qualified Data.IntMap as M

data AnySignal where
  AnySignal :: SigLike a => a -> AnySignal

class SigLike a where
  -- | Determine in which order this signal, and all its listeners, are
  --   supposed to fire.
  mark         :: Int -> a -> IO Int
  -- | Activate signals in the order determined by the last call to mark.
  collect      :: M.IntMap AnySignal -> a -> IO (M.IntMap AnySignal)
  -- | Add b as a listener of a.
  listensTo    :: AnySignal -> a -> IO ()
  -- | Get all signals listening to this signal.
  getListeners :: a -> IO [AnySignal]
  -- | Evaluate the signal, update its output and mark all listeners as either
  --   "should fire" or "should not fire" the next time it's poked.
  poke         :: a -> IO ()
  -- | Set whether the signal should fire on next poke or not.
  setFire      :: a -> Bool -> IO ()

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
    dependencies :: [(Listener, AnySignal)],
    -- | The last place in the trigger order where this signal will trigger.
    --   This is used internally to ensure signals are only triggered once,
    --   after all of their dependencies have been met.
    order        :: IORef Int,
    -- | Should I fire the next time I'm poked?
    shouldFire   :: IORef Bool
  }

instance SigLike AnySignal where
  setFire (AnySignal s) f =
    setFire s f
  poke (AnySignal s) =
    poke s
  collect m (AnySignal s) =
    collect m s
  mark n (AnySignal s) =
    mark n s
  listensTo listener (AnySignal speaker) =
    listener `listensTo` speaker
  getListeners (AnySignal s) =
    getListeners s

instance SigLike (Signal a) where
  setFire s f = do
    writeIORef (shouldFire s) f

  mark num sig = do
    writeIORef (order sig) num
    ls <- readIORef (listeners sig)
    markAll (num+1) ls
    where
      markAll n (x:xs) = do
        n' <- mark n x
        markAll n' xs
      markAll n _ =
        return n
  
  collect m sig = do
    m' <- M.insert <$> readIORef (order sig) <*> pure (AnySignal sig) <*> pure m
    ls <- getListeners sig
    foldM collect m' ls

  poke sig = do
    firingIsAppropriate <- readIORef (shouldFire sig)
    writeIORef (shouldFire sig) False
    when firingIsAppropriate $ do
      let out = output sig
      old <- readIORef out
      new <- trigger sig
      writeIORef out new
      ls <- getListeners sig
      when (propagate sig old new) $ do
        mapM_ (\l -> setFire l True) ls

  listensTo l sig = modifyIORef (listeners sig) $ \ls -> l : ls
  getListeners    = readIORef . listeners

-- | Activate a signal, recursively notifying all listeners.
--   The algorithm for the cascaded update is a bit tricky; as signals,
--   particularly at the end of a chain, may contain side effects, we don't
--   want them to fire more than at most once per event. Thus, we can't just
--   recursively activate all of a signal's listeners, because a signal down
--   the line may depend on this signal through more than one path.
--   For instance:
-- @
--   (input1, sig1) <- source 0
--   (input2, sig2) <- source 0
--   let multi = (*) <$> sig1 <*> sig2
--   start $ perform $ (\a b -> print (a + b)) <$> sig1 <*> multi
--   push 10 input1
-- @
--  If we were to just recursively propagate signals we would call print twice;
--  once when triggered by sig1, and once when triggered by multi, which is
--  also triggered by sig1.
--
--  To avoid this, we recursively traverse all listeners of the originating
--  signal, marking each one with an ever increasing ordering value. If a
--  signal is encountered more than once, its old ordering value is overwritten
--  with the new, higher, one.
--
--  Then, we collect all the signals into an IntMap, keyed on their ordering
--  value. This ensures that no signal will appear in the firing list more than
--  once, and it will always fire after all of its dependencies.
--
--  Finally, we mark the originating signal for firing on next poke, and poke
--  it. It will update, and will mark all of its listeners for firing iff its
--  propagation function determines that the signal should be propagated.
--  We then proceed to poke all of the other signals in the firing list, and
--  the once that have had their "fire on next poke" status set by another
--  signal will fire.
--  After being poked, all signals immediately reset to "no, don't fire on
--  next poke," to get ready for the next event.
activate :: SigLike a => a -> IO ()
activate sig = do
  _ <- mark 0 sig
  sigs <- collect M.empty sig
  setFire sig True
  mapM_ (poke . snd) $ M.toAscList sigs
  return ()

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
      trigger      = trigger sig >>= return . f,
      output       = newRef sig undefined,
      listeners    = newRef sig [],
      propagate    = alwaysPropagate,
      order        = newRef sig 0,
      shouldFire   = newRef sig False,
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
      order        = newRef x 0,
      shouldFire   = newRef x False,
      dependencies = []
    }

  sig <*> x = Signal {
      trigger      = app,
      output       = newRefIO x app,
      listeners    = newRef x [],
      propagate    = alwaysPropagate,
      order        = newRef x 0,
      shouldFire   = newRef x False,
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
                   order        = newRef sig 0,
                   shouldFire   = newRef sig False,
                   dependencies = dependencies sig}

-- | A buffered signal never activates its listeners.
buffered :: Signal a -> Signal a
buffered sig = self
  where
    self = Signal {trigger      = readIORef (output sig),
                   output       = newRefIO sig (readIORef $ output sig),
                   listeners    = newRef sig [],
                   propagate    = neverPropagate,
                   order        = newRef sig 0,
                   shouldFire   = newRef sig False,
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
                   order        = newRef sig 0,
                   shouldFire   = newRef sig False,
                   dependencies =
                     (Other$AnySignal self, AnySignal sig) : map (unSelf sig) (dependencies sig)}

-- | Create a signal source. A source lets you raise signals by 'push'ing
--   values into it. Each pushed value will trigger the corresponding signal.
source :: a -> IO (Source a, Signal a)
source initial = do
  out <- newIORef initial
  ls <- newIORef []
  l <- newIORef 0
  fire <- newIORef False
  let self = Signal {
          trigger      = readIORef out,
          output       = out,
          listeners    = ls,
          propagate    = alwaysPropagate,
          order        = l,
          shouldFire   = fire,
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
alwaysPropagate _ _ = True

neverPropagate :: a -> a -> Bool
neverPropagate _ _ = False
