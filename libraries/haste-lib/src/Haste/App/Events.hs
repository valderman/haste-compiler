{-# LANGUAGE TypeFamilies, GADTs, FlexibleInstances, ForeignFunctionInterface,
             CPP #-}
-- | Event handlers for Haste.App. If you're using Haste.App, you should use
--   the functions provided by this module rather than the ones from
--   Haste.Events.
module Haste.App.Events (
    module Core,
    ClientCallback, onEvent
  ) where
import Haste.App.Client
import Haste.Concurrent
import Haste.DOM.Core
import qualified Haste.Events.Core as E
import qualified Haste.Events.Core as Core hiding (onEvent)

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
onEvent :: (IsElem el, E.Event evt, ClientCallback a)
        => el
        -> evt
        -> (E.EventData evt -> Client ())
        -> Client E.HandlerInfo
onEvent e evt f = do
    cs <- get id
    liftIO $ e `E.onEvent` evt $ concurrent . runClientCIO cs . f
