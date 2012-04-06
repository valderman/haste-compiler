{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Haste.Reactive.Signal (
  Signal, start, lazy, buffered, new, perform, async, initially,
  pipe, pipeWhen, push, triggers) where
import Control.Applicative
import Control.Monad
import Data.IORef
import qualified Data.IntMap as M

data Signal a where
  -- Applicative primitives; functor instance is also expressed in these.
  Pure      :: a -> Signal a
  App       :: Signal (a -> b) -> Signal a -> Signal b
  -- Perform rather than return the result of a signal returning IO.
  Join      :: Signal (IO a) -> Signal a
  -- These all have corresponding exported functions, with explanations.
  New       :: IO (Signal a) -> Signal a
  Pipe      :: IORef (Maybe a) -> IORef [AnySig] -> Signal a
  Lazy      :: Eq a => Signal a -> Signal a
  Buffered  :: Signal a -> Signal a
  Async     :: Signal (Pipe a -> IO ()) -> Signal a
  Initially :: a -> Signal a -> Signal a
  Trigger   :: Signal a -> Signal b -> Signal b

instance Functor Signal where
  fmap f s = App (Pure f) s

instance Applicative Signal where
  pure  = Pure
  (<*>) = App

-- | Most of the methods in SigLike deal with updating signals.
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
class SigLike a where
  mark     :: Int -> a -> IO Int
  collect  :: M.IntMap AnySig -> a -> IO (M.IntMap AnySig)
  poke     :: a -> IO ()
  setFire  :: a -> Bool -> IO ()
  getLstns :: a -> IO [AnySig]
  addLstnr :: AnySig -> a -> IO ()

instance SigLike (Sig a) where
  setFire sig f = do
    writeIORef (shouldFire sig) f

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
    m' <- M.insert <$> readIORef (order sig) <*> pure (AnySig sig) <*> pure m
    ls <- getLstns sig
    foldM collect m' ls

  poke sig = do
    firingIsAppropriate <- readIORef (shouldFire sig)
    writeIORef (shouldFire sig) False
    when firingIsAppropriate $ do
      oldVal <- readIORef (output sig)
      newVal <- action sig
      writeIORef (output sig) (Just newVal)
      ls <- getLstns sig
      when (pushWhen sig oldVal (Just newVal)) $ do
        mapM_ (\l -> setFire l True) ls

  addLstnr l s = do
    ls <- readIORef (listeners s)
    writeIORef (listeners s) (l:ls)
  
  getLstns = readIORef . listeners

instance SigLike AnySig where
  mark n (AnySig s)     = mark n s
  collect m (AnySig s)  = collect m s
  poke (AnySig s)       = poke s
  setFire (AnySig s) f  = setFire s f
  addLstnr l (AnySig s) = addLstnr l s
  getLstns (AnySig s)   = getLstns s

data AnySig where
  AnySig :: SigLike a => a -> AnySig

-- | The role of the Sig data type is to keep track of values associated with
--   various signals internally.
data Sig a = Sig {
    action     :: IO a,
    listeners  :: IORef [AnySig],
    output     :: IORef (Maybe a),
    pushWhen   :: Maybe a -> Maybe a -> Bool,
    shouldFire :: IORef Bool,
    order      :: IORef Int,
    deps       :: [AnySig]
  }

-- | A pipe is an event generating communications channel. If data is 'push'ed
--   into it, it appears at the output of the corresponding signal, which also
--   triggers. Pushing data into a pipe is the only way to trigger a chain of
--   events.
data Pipe a = P {
    pipeout   :: IORef (Maybe a),
    pipelstns :: IORef [AnySig],
    pipepush  :: Maybe a -> Maybe a -> Bool
  }

-- | Cause signal B to trigger whenever signal A does. Sometimes, the only
--   thing of value a signal does is to cause a side effect to happen, one
--   which does not depend on any inputs except for setting the signal in
--   motion. That is the need triggers fills.
triggers :: Signal a -> Signal b -> Signal b
triggers t (Async sig) = Async (Trigger t sig)
triggers t sig         = Trigger t sig

-- | Set an initial value for a signal. This is the value that will be read
--   by others before the signal has triggered.
initially :: a -> Signal a -> Signal a
initially = Initially

-- | Create an asynchronous signal. The computation returned by the incoming
--   signal must set up a callback (or something else) that pushes a value
--   into the provided pipe when it's time for the signal to continue.
--   It's primarily meant as a building block for more high level signals.
--   This example creates and starts a signal that reacts to input by throwing
--   up a dialog box after two seconds.
-- @
--   main = do
--     (p, sig) <- pipe ""
--     sink alert later
--     push "Two seconds later!" p
--     where
--       later = async $ (\str pipe -> setTimeout 2000 (push str pipe)) <$> sig
-- @
async :: Signal (Pipe a -> IO ()) -> Signal a
async = Async

-- | Turn a signal lazy. A lazy signal only propagates when its input actually
--   changes.
lazy :: Eq a => Signal a -> Signal a
lazy = Lazy

-- | Buffer a signal. A buffered signal never triggers its listeners.
buffered :: Signal a -> Signal a
buffered = Buffered

-- | Create a new signal using a signal generator.
new :: IO (Signal a) -> Signal a
new = New

-- | If a signal returns an IO action, perform it rather than return it.
--   This might go away in the future in favour of Signal instances of Monad
--   and MonadIO.
perform :: Signal (IO a) -> Signal a
perform = Join

-- | Create a 'Pipe'.
pipe :: a -> IO (Pipe a, Signal a)
pipe = pipeWhen (\_ _ -> True)

-- | Create a pipe with a custom policy for when the push should propagate.
pipeWhen :: (Maybe a -> Maybe a -> Bool) -> a -> IO (Pipe a, Signal a)
pipeWhen pushwhen initial = do
  out <- newIORef (Just initial)
  ls <- newIORef []
  return (P out ls pushwhen, Pipe out ls)

-- | Push data into a 'Pipe'.
push :: a -> Pipe a -> IO ()
push val (P ref ls pushwhen) = do
  old <- readIORef ref
  writeIORef ref (Just val)
  when (pushwhen old (Just val)) $ do
    lstns <- readIORef ls
    foldM_ mark 0 lstns
    sigs <- foldM collect M.empty lstns
    mapM_ (flip setFire True) lstns
    mapM_ (poke . snd) $ M.toAscList sigs

-- | Create a new internal signal builder structure thingy.
mkSig :: IO a -> Maybe a -> IO (Sig a)
mkSig act initial = do
  out <- newIORef initial
  ls <- newIORef []
  ord <- newIORef 0
  fire <- newIORef False
  return $ Sig {action     = act,
                listeners  = ls,
                output     = out,
                deps       = [],
                order      = ord,
                shouldFire = fire,
                pushWhen   = \_ _ -> True}

-- | Compile a signal, hook up all its dependencies and activate it.
start :: Signal a -> IO ()
start sig = do
  s <- compile sig
  mapM_ (addLstnr (AnySig s)) (deps s)

-- | Compile a signal. Some hooking up of signals happens within compile, with
--   the final ones happen in 'start'.
compile :: Signal a -> IO (Sig a)
compile (Pure x) = do
  mkSig (return x) (Just x)
compile (Join sigm) = do
  sigm' <- compile sigm
  sig <- mkSig (join $ action sigm') Nothing
  return sig {deps = deps sigm'}
compile (New create) = do
  create >>= compile
compile (Pipe out ls) = do
  p <- mkSig (readIORef out >>= \(Just x) -> return x) Nothing
  return p {listeners = ls,
            output    = out}
compile (App f x) = do
  f' <- compile f
  x' <- compile x
  -- x is "done" - register all its dependencies
  mapM_ (addLstnr (AnySig x')) (deps x')
  
  s <- mkSig (act f' x') Nothing
  return s {deps = AnySig x' : deps f'}
  where
    act f' x' = do
      f'' <- action f'
      Just x'' <- readIORef $ output x'
      return $ f'' x''
compile (Trigger x sig) = do
  x' <- compile x
  sig' <- compile sig
  -- x is done, register dependencies
  mapM_ (addLstnr (AnySig x')) (deps x')
  return sig' {deps = AnySig x' : deps sig'}
compile (Lazy sig) = do
  sig' <- compile sig
  lzy <- mkSig (action sig') Nothing
  return lzy {pushWhen = (/=),
              deps     = AnySig sig' : deps sig'}
compile (Buffered sig) = do
  sig' <- compile sig
  bfd <- mkSig (action sig') Nothing
  return bfd {pushWhen = \_ _ -> False,
              deps     = AnySig sig' : deps sig'}
compile (Async setup) = do
  -- Async is less tricky than you'd think; we set up the incoming signal as
  -- an independent signal chain, which is responsible for setting a callback
  -- or similar that pushes a value into the provided pipe when appropriate.
  (p,s) <- pipe undefined
  start $ perform $ setup <*> pure p
  compile s
compile (Initially x sig) = do
  sig' <- compile sig
  writeIORef (output sig') (Just x)
  return sig'
