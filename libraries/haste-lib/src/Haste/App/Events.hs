{-# LANGUAGE TypeFamilies, GADTs, FlexibleInstances, ForeignFunctionInterface,
             CPP #-}
-- | Event handlers for Haste.App. If you're using Haste.App, you should use
--   the functions provided by this module rather than the ones from
--   Haste.Callback.
module Haste.App.Events (
    ClientCallback, CB.Event (..),
    onEvent, setTimeout, CB.evtName
  ) where
import qualified Haste.Callback as CB
import Haste.App.Client
import Haste.Concurrent
import Haste.DOM

-- | Bake a value of type a -> ... -> Client b into a -> ... -> IO b
class ClientCallback a where
  type T a
  cbify :: ClientState -> a -> T a

instance ClientCallback (Client ()) where
  type T (Client ()) = IO ()
  cbify cs = concurrent . runClientCIO cs

instance ClientCallback b => ClientCallback (a -> b) where
  type T (a -> b) = a -> T b
  cbify cs f = \x -> cbify cs (f x)

-- | Set a handler for a given event.
onEvent :: ClientCallback a => Elem -> CB.Event Client a -> a -> Client ()
onEvent e evt f = do
    cs <- get id
    _ <- liftIO . CB.jsSetCB e (CB.evtName evt) . CB.mkCallback $! cbify cs f
    return ()

-- | Wrapper for window.setTimeout; execute the given computation after a delay
--   given in milliseconds.
setTimeout :: Int -> Client () -> Client ()
setTimeout delay cb = do
  cs <- get id
  liftIO $ CB.jsSetTimeout delay (CB.mkCallback $! cbify cs cb)
