{-# LANGUAGE OverloadedStrings, TypeFamilies, TupleSections, CPP #-}
-- | Events relating to mouse clicks and movement.
module Haste.Events.MouseEvents (
    MouseEvent (..), MouseData (..), MouseButton (..)
  ) where
import Haste.Object
import Haste.JSType
import Haste.Events.Core
import Haste.Foreign
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Maybe

data MouseButton = MouseLeft | MouseMiddle | MouseRight
  deriving (Show, Eq, Enum)

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
              <*> ((,,) <$> (e # "deltaX" >>= fmap fromJust . asNumber)
                        <*> (e # "deltaY" >>= fmap fromJust . asNumber)
                        <*> (e # "deltaZ" >>= fmap fromJust . asNumber))

  eventData _ e =
    MouseData <$> jsGetMouseCoords e
              <*> (e # "button" >>= fmap (fmap (toEnum . convert)) . asNumber)
              <*> pure (0,0,0)

{-# NOINLINE jsGetMouseCoords #-}
jsGetMouseCoords :: JSObj -> IO (Int, Int)
jsGetMouseCoords = ffi "jsGetMouseCoords"
