{-# LANGUAGE TypeFamilies, GADTs, FlexibleInstances, ForeignFunctionInterface,
             CPP #-}
-- | Event handlers for Haste.App. If you're using Haste.App, you should use
--   the functions provided by this module rather than the ones from
--   Haste.Events.
module Haste.App.Events (
    module Events, onEvent
  ) where
import Haste.App.Client
import Haste.Concurrent
import Haste.DOM.Core
import qualified Haste.Events as E
import qualified Haste.Events as Events hiding (onEvent)

-- | Set a handler for a given event.
onEvent :: (IsElem el, E.Event evt)
        => el
        -> evt
        -> (E.EventData evt -> Client ())
        -> Client E.HandlerInfo
onEvent e evt f = do
    cs <- get id
    liftIO $ e `E.onEvent` evt $ concurrent . runClientCIO cs . f
