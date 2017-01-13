{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies,
             EmptyDataDecls #-}
-- | Concurrency for Haste. Includes MVars, forking, Ajax and more.
module Haste.Concurrent (
    module Haste.Concurrent.Monad,
    wait, withResult
  ) where
import Haste.Concurrent.Monad
import Haste.Timer

-- | Wait for n milliseconds.
wait :: Int -> CIO ()
wait ms = do
  v <- newEmptyMVar
  _ <- liftIO $ setTimer (Once ms) $ concurrent $ putMVar v ()
  takeMVar v

-- | When the given concurrent computation is done, pass the result to the
--   given callback.
withResult :: CIO a -> (a -> IO ()) -> IO ()
withResult m f = concurrent $ m >>= liftIO . f
