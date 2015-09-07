{-# LANGUAGE OverloadedStrings, TypeFamilies, CPP #-}
-- | Events relating to touch input.
module Haste.Events.TouchEvents (
    TouchEvent (..), TouchData (..), Touch (..)
  ) where
import Haste.Prim.Any
import Haste.Foreign
import Haste.Events.Core
import Haste.DOM.Core
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

data TouchEvent
  = TouchStart
  | TouchMove
  | TouchEnd
  | TouchCancel

-- | Event data for touch events.
data TouchData = TouchData {
    -- | All fingers currently on the screen.
    touches        :: [Touch],

    -- | All fingers on the DOM element receiving the event.
    targetTouches  :: [Touch],

    -- | All fingers involved in the current event. For instance, fingers
    --   removed for the 'TouchEnd' event.
    changedTouches :: [Touch]
  }

-- | A finger touching the screen.
data Touch = Touch {
    -- | Unique identifier for this touch.
    identifier    :: !Int,

    -- | Target element of the touch.
    target        :: !Elem,

    -- | Page coordinates of the touch, including scroll offsets.
    pageCoords    :: !(Int, Int),

    -- | Page coordinates of the touch, excluding scroll offsets.
    clientCoords  :: !(Int, Int),

    -- | Screen coordinates of the touch.
    screenCoords  :: !(Int, Int)
  }

instance FromAny Touch where
  fromAny t =
    Touch <$> get t "identifier"
          <*> get t "target"
          <*> ((,) <$> get t "pageX"   <*> get t "pageY")
          <*> ((,) <$> get t "clientX" <*> get t "clientY")
          <*> ((,) <$> get t "screenX" <*> get t "screenY")

instance Event TouchEvent where
  type EventData TouchEvent = TouchData
  eventName TouchStart  = "touchstart"
  eventName TouchMove   = "touchmove"
  eventName TouchEnd    = "touchend"
  eventName TouchCancel = "touchcancel"

  eventData _ e = do
    ts <- get e "touches"
    (cts, tts) <- getTIDs e
    return $ TouchData {
        touches = ts,
        changedTouches = filter ((`elem` cts) . identifier) ts,
        targetTouches = filter ((`elem` tts) . identifier) ts
      }

-- | Get the touch IDs of all touches for changedTouches and targetTouches.
getTIDs :: JSAny -> IO ([Int], [Int])
getTIDs = ffi "(function(e) {\
  var len = e.changedTouches.length;\
  var chts = new Array(len);\
  for(var i = 0; i < len; ++i) {chts[i] = e.changedTouches[i].identifier;}\
  var len = e.targetTouches.length;\
  var tts = new Array(len);\
  for(var i = 0; i < len; ++i) {tts[i] = e.targetTouches[i].identifier;}\
  return [chts, tts];})"
