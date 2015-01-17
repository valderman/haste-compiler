{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
-- | Basic events: load, unload, focus, submit, etc.
module Haste.Events.BasicEvents (BasicEvent (..)) where
import Haste.Object
import Haste.JSType
import Haste.Events.Core
import Control.Applicative
import Data.Maybe

data BasicEvent
  = Load
  | Unload
  | Change
  | Focus
  | Blur
  | Submit

instance Event BasicEvent where
  type EventData BasicEvent = ()
  eventName Load   = "load"
  eventName Unload = "unload"
  eventName Change = "change"
  eventName Focus  = "focus"
  eventName Blur   = "blur"
  eventName Submit = "submit"
  eventData _ _    = return ()
