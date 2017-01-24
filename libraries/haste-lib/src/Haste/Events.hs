-- | Event handling for Haste.
module Haste.Events (
    -- * Core event functionality
    Event (..), MonadEvent (..),
    HandlerInfo,
    unregisterHandler, onEvent, preventDefault, stopPropagation,

    -- * Basic events with no arguments
    BasicEvent (..),

    -- * Keyboard-related events
    KeyEvent (..), KeyData (..), mkKeyData,

    -- * Mouse-related events
    MouseEvent (..), MouseData (..), MouseButton (..),

    -- * Touch-related events
    TouchEvent (..), TouchData (..), Touch (..),

    -- * Messaging events and @postMessage@ API.
    MessageEvent (..), MessageData (..), Window,
    postMessage, window, getContentWindow, fromAny
  ) where
import Haste.Events.Core as Core
import Haste.Events.BasicEvents as BasicEvents
import Haste.Events.KeyEvents as KeyEvents
import Haste.Events.MouseEvents as MouseEvents
import Haste.Events.TouchEvents as TouchEvents
import Haste.Events.MessageEvents as MessageEvents
