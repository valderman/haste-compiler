-- | Event handling for Haste.
module Haste.Events (
    -- * Core event functionality
    Event (..), MonadEvent (..),
    HandlerInfo,
    unregisterHandler, onEvent, preventDefault,

    -- * Basic events with no arguments
    BasicEvent (..),

    -- * Keyboard-related events
    KeyEvent (..), KeyData (..), mkKeyData,

    -- * Mouse-related events
    MouseEvent (..), MouseData (..), MouseButton (..)
  ) where
import Haste.Events.Core as Core
import Haste.Events.BasicEvents as BasicEvents
import Haste.Events.KeyEvents as KeyEvents
import Haste.Events.MouseEvents as MouseEvents
