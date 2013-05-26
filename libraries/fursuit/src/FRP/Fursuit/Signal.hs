{-# LANGUAGE GADTs, BangPatterns, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module FRP.Fursuit.Signal (Signal (..), Pipe (..), sink) where
import Data.IORef
import System.IO.Unsafe
import Control.Applicative
import qualified Data.IntMap as M
import Data.Maybe

type SinkID = Int
type Origin = Bool
type Sig a = IO (Maybe (a, Origin))
type SinkList = M.IntMap (IO ())

data Pipe a = P {
    piperef :: IORef (Maybe a),
    cbref   :: IORef (M.IntMap (IO ())),
    origref :: IORef Origin
  }

-- New is implemented as a thin wrapper around unsafePerformIO, to make sure
-- it's only evaluated once and never outside of sink.
data Signal a where
  App    :: Signal (a -> b) -> Signal a -> Signal b
  Pure   :: a -> Signal a
  Pipe   :: IORef (Maybe a) -> IORef SinkList -> IORef Origin -> Signal a
  Filter :: (a -> Bool) -> Signal a -> Signal a
  Accum  :: a -> Signal (a -> a) -> Signal a
  New    :: Signal a -> Signal a
  Union  :: Signal a -> Signal a -> Signal a

{-# NOINLINE sinkIDs #-}
sinkIDs :: IORef SinkID
sinkIDs = unsafePerformIO $ newIORef 0

-- | Generate a new sink ID, and update the global list of such IDs.
newSinkID :: IO SinkID
newSinkID = do
  sid <- readIORef sinkIDs
  writeIORef sinkIDs $! sid+1
  return sid

-- | Attach a signal to an actuator.
sink :: (a -> IO ()) -> Signal a -> IO ()
sink act sig = do
  sig' <- compile sig >>= return . mkSnk act
  sid <- newSinkID
  mapM_ (\sinks -> modifyIORef sinks (M.insert sid sig')) (sources sig)
  where
    mkSnk action signal = do
      result <- signal
      case result of
        Just (val, actuallyHappened) | actuallyHappened -> action val
        _                                               -> return ()

    -- Find all sources for this signal
    sources :: Signal a -> [IORef SinkList]
    sources (App f x)      = sources f ++ sources x
    sources (Pure _)       = []
    sources (Pipe _ src _) = [src]
    sources (Filter _ s)   = sources s
    sources (Accum _ s)    = sources s
    sources (New s)        = sources s
    sources (Union a b)    = sources a ++ sources b
    
    -- Compile the signal into an IO action we can trigger whenever one of its
    -- sources gets a signal.
    compile :: Signal a -> IO (Sig a)
    compile (App sf sx) = do
      f <- compile sf
      x <- compile sx
      return (f `appS` x)
    compile (Pure x) = do
      return (return $ Just (x, False))
    compile (Pipe value _ origin) = do
      return $ do
        mval <- readIORef value
        orig <- readIORef origin
        return $ mval >>= \val -> return (val, orig)
    compile (Filter predicate s) = do
      s' <- compile s
      ms <- s'
      lastGood <- case ms of
        Just (initial, _) | predicate initial -> newIORef (Just initial)
        _                                     -> newIORef Nothing
      s'' <- compile s
      return (fltS predicate lastGood s'')
    compile (Accum initially s) = do
      s' <- compile s
      ref <- newIORef initially
      return (accS ref s')
    compile (New signal) = do
      compile signal
    compile (Union a b) = do
      a' <- compile a
      b' <- compile b
      -- Prefer to initialize with the value of the left signal.
      maval <- a'
      initial <- case maval of
        Just (x, _) -> return (Just x)
        _           -> do
          mbval <- b'
          case mbval of
            Just (x, _) -> return (Just x)
            _           -> return Nothing
      prev <- newIORef initial
      return $ uniS a' b' prev

-- | Union of two events. Both events are always evaluated.
uniS :: Sig a -> Sig a -> IORef (Maybe a) -> Sig a
uniS sa sb prevref = do
  ma <- sa
  mb <- sb
  prev <- readIORef prevref
  case listToMaybe $ filter snd $ catMaybes [ma, mb] of
    Nothing -> do
      return $ fmap (, False) prev
    val -> do
      writeIORef prevref (fmap fst val)
      return val

-- | Basically ap or <*>, but takes the origin indicator into account.
appS :: Sig (a -> b) -> Sig a -> Sig b
appS sf sx = do
  mf <- sf
  mx <- sx
  return $ do
    (f, origf) <- mf
    (x, origx) <- mx
    return (f x, origf || origx)

-- | filterS, with a memo reference for the last value that passed through.
fltS :: (a -> Bool) -> IORef (Maybe a) -> Sig a -> Sig a
fltS predicate lastGood signal = do
  msig <- signal
  case msig of
    Just (val, orig) -> do
      if predicate val && orig
        then do
          writeIORef lastGood (Just val)
          return $ Just (val, True)
        else do
          mlast <- readIORef lastGood
          case mlast of
            Just lastVal -> return $ Just (lastVal, False)
            _            -> return Nothing
    _ -> do
      return Nothing

-- | accumS
accS :: IORef a -> Sig (a -> a) -> Sig a
accS lastRef sf = do
  mf <- sf
  case mf of
    Just (f, orig) -> do
      if orig
        then do
          val <- readIORef lastRef
          let !x = f val
          writeIORef lastRef x
          return $ Just (x, True)
        else do
          lastVal <- readIORef lastRef
          return $ Just (lastVal, False)
    Nothing -> do
          -- Even if upstream can't compute its value, Accum can.
          lastVal <- readIORef lastRef
          return $ Just (lastVal, False)

instance Functor Signal where
  fmap f x = pure f <*> x

instance Applicative Signal where
  pure  = Pure
  (<*>) = App
