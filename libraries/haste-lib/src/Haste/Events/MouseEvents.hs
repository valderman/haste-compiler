{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
-- | Events relating to mouse clicks and movement.
module Haste.Events.MouseEvents (MouseEvent (..), MouseData (..)) where
import Haste.Object
import Haste.JSType
import Haste.Events.Core
import Control.Applicative
import Data.Maybe

data MouseButton = MouseLeft | MouseMiddle | MouseRight
  deriving (Show, Eq, Enum)

-- | Event data for mouse events.
data MouseData = MouseData {
    -- | X mouse coordinate.
    mouseX      :: !Int,
    -- | Y mouse coordinate.
    mouseY      :: !Int,
    -- | Pressed mouse button, if any.
    mouseButton :: !(Maybe MouseButton),
    -- | Mouse wheel delta X. Always 0 except for 'Wheel'.
    mouseWheelX :: !Double,
    -- | Mouse wheel delta Y. Always 0 except for 'Wheel'.
    mouseWheelY :: !Double,
    -- | Mouse wheel delta Z. Always 0 except for 'Wheel'.
    mouseWheelZ :: !Double
  }

data MouseEvent
  = Click
  | DblClick
  | MouseDown
  | MouseUp
  | MouseMove
  | MouseOver
  | MouseOut
  | Wheel

instance Event MouseEvent where
  type EventData MouseEvent = MouseData
  eventName Click     = "click"
  eventName DblClick  = "dblclick"
  eventName MouseDown = "mousedown"
  eventName MouseUp   = "mouseup"
  eventName MouseMove = "mousemove"
  eventName MouseOver = "mouseover"
  eventName MouseOut  = "mouseout"
  eventName Wheel     = "wheel"
  eventData Wheel e =
    MouseData <$> (e # "pageX" >>= fmap (convert . fromJust) . asNumber)
              <*> (e # "pageY" >>= fmap (convert . fromJust) . asNumber)
              <*> pure Nothing
              <*> (e # "deltaX" >>= fmap fromJust . asNumber)
              <*> (e # "deltaY" >>= fmap fromJust . asNumber)
              <*> (e # "deltaZ" >>= fmap fromJust . asNumber)

  eventData _ e =
    MouseData <$> (e # "pageX" >>= fmap (convert . fromJust) . asNumber)
              <*> (e # "pageY" >>= fmap (convert . fromJust) . asNumber)
              <*> (e # "button" >>= fmap (fmap (toEnum . convert)) . asNumber)
              <*> pure 0
              <*> pure 0
              <*> pure 0
