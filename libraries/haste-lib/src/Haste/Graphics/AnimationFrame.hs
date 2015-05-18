{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
-- | Request and cancel animation frames from the browser.
--   Straightforward bindings to the corresponding DOM interface.
module Haste.Graphics.AnimationFrame (
    FrameRequest, HRTimeStamp,
    requestAnimationFrame,
    cancelAnimationFrame
  ) where
import Haste.Foreign
import Haste.Performance

-- | Handle to a previously issued request for an animation frame.
--   Only useful together with 'cancelAnimationFrame'.
newtype FrameRequest = FrameRequest JSAny deriving (ToAny, FromAny)

-- | Request a function to be called by the browser before the next repaint.
--   Paints generally happen in tune with the user's monitor refresh rate,
--   which usually means at 60 FPS.
--
--   Do note that you need to request *each* animation callback you plan to
--   use, similar to @setTimeout@ as opposed to @setInterval@, as they are not
--   recurring.
requestAnimationFrame :: (HRTimeStamp -> IO ()) -> IO FrameRequest
requestAnimationFrame = ffi "window.requestAnimationFrame"

-- | Cancel an animation callback previously requested by
--   'requestAnimationFrame'.
cancelAnimationFrame :: FrameRequest -> IO ()
cancelAnimationFrame = ffi "window.cancelAnimationFrame"
