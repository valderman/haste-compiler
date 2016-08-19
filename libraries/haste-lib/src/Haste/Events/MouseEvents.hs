{-# LANGUAGE OverloadedStrings, TypeFamilies, TupleSections, CPP #-}
-- | Events relating to mouse clicks and movement.
module Haste.Events.MouseEvents (
    MouseEvent (..), MouseData (..), MouseButton (..)
  ) where
import Haste.Events.Core
import Haste.Prim.Foreign

data MouseButton = MouseLeft | MouseMiddle | MouseRight
  deriving (Show, Eq, Enum)

instance FromAny MouseButton where
  fromAny = fmap toEnum . fromAny

-- | Event data for mouse events.
data MouseData = MouseData {
    -- | Mouse coordinates.
    mouseCoords      :: !(Int, Int),
    -- | Pressed mouse button, if any.
    mouseButton      :: !(Maybe MouseButton),
    -- | (x, y, z) mouse wheel delta. Always all zeroes except for 'Wheel'.
    mouseWheelDeltas :: !(Double, Double, Double)
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
    MouseData <$> jsGetMouseCoords e
              <*> pure Nothing
              <*> ((,,) <$> (get e "deltaX")
                        <*> (get e "deltaY")
                        <*> (get e "deltaZ"))

  eventData _ e =
    MouseData <$> jsGetMouseCoords e
              <*> get e "button"
              <*> pure (0,0,0)

jsGetMouseCoords :: JSAny -> IO (Int, Int)
jsGetMouseCoords = ffi "jsGetMouseCoords"
