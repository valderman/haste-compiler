{-# LANGUAGE CPP #-}
-- | Locking for Fursuit - to enable running it safely on multithreaded
--   platforms.
module FRP.Fursuit.Locking (withFursuitLock) where

#ifdef __HASTE__

-- | The Haste version is just a no-op; no threads in the browser!
withFursuitLock :: IO a -> IO a
withFursuitLock = id

#else

import Control.Concurrent.MVar
import System.IO.Unsafe

{-# NOINLINE fursuitLock #-}
-- | While this may seem insanely unsafe due to blackholing, it's actually
--   perfectly OK as of GHC 7.4.2, when CAFs were made atomic.
--   (http://hackage.haskell.org/trac/ghc/ticket/5558)
fursuitLock :: MVar ()
fursuitLock = unsafePerformIO $! newMVar ()

-- | The GHC version is really stupid: smack a big lock around the entire
--   write operation. It works, but it's neither elegant nor efficient.
withFursuitLock :: IO a -> IO a
withFursuitLock action = do
  _ <- takeMVar fursuitLock
  res <- action
  putMVar fursuitLock ()
  return res

#endif
