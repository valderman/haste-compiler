{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
-- | Audio related events.
module Haste.Audio.Events where
import Haste.Events.Core

data AudioEvent
  = AudioEnded       -- ^ Audio playback ended.
  | AudioError       -- ^ There was some kind of error.
  | AudioPaused      -- ^ Audio paused.
  | AudioResumed     -- ^ Resumed playing after pause.
  | AudioPlaying     -- ^ Audio started playing, initially or after pause.
  | AudioSeekBegins  -- ^ Seek operation starts.
  | AudioSeekEnds    -- ^ Seek operation completes.
  | AudioTimeUpdate  -- ^ Audio object't current time changed.
  | AudioProgress    -- ^ Progress was made downloading audio.
  | AudioStalled     -- ^ Audio download stalled.
  | AudioLoadStart   -- ^ Start downloading audio.
  | AudioLoadSuspend -- ^ Finished or paused downloading audio.

instance Event AudioEvent where
  type EventData AudioEvent = ()
  eventName AudioEnded       = "ended"
  eventName AudioError       = "error"
  eventName AudioPaused      = "pause"
  eventName AudioResumed     = "play"
  eventName AudioPlaying     = "playing"
  eventName AudioSeekBegins  = "seeking"
  eventName AudioSeekEnds    = "seeked"
  eventName AudioTimeUpdate  = "timeupdate"
  eventName AudioProgress    = "progress"
  eventName AudioStalled     = "stalled"
  eventName AudioLoadStart   = "loadstart"
  eventName AudioLoadSuspend = "suspend"
  eventData _ _ = return ()
