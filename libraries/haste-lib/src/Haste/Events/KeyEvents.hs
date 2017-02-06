{-# LANGUAGE OverloadedStrings, TypeFamilies, CPP #-}
-- | Events relating to keyboard input.
module Haste.Events.KeyEvents (KeyEvent (..), KeyData (..), mkKeyData) where
import Haste.Prim.Any
import Haste.Events.Core

-- | Event data for keyboard events.
data KeyData = KeyData {
    keyCode  :: !Int,
    keyCtrl  :: !Bool,
    keyAlt   :: !Bool,
    keyShift :: !Bool,
    keyMeta  :: !Bool
  } deriving (Show, Eq)

-- | Build a 'KeyData' object for the given key, without any modifier keys
--   pressed.
mkKeyData :: Int -> KeyData
mkKeyData n = KeyData {
    keyCode  = fromIntegral n,
    keyCtrl  = False,
    keyAlt   = False,
    keyShift = False,
    keyMeta  = False
  }

-- | Num instance for 'KeyData' to enable pattern matching against numeric
--   key codes.
instance Num KeyData where
  fromInteger = mkKeyData . fromInteger
  a + b    = a {keyCode = keyCode a  +  keyCode b}
  a * b    = a {keyCode = keyCode a  *  keyCode b}
  a - b    = a {keyCode = keyCode a  -  keyCode b}
  negate a = a {keyCode = negate $ keyCode a}
  signum a = a {keyCode = signum $ keyCode a}
  abs a    = a {keyCode = abs $ keyCode a}

data KeyEvent
  = KeyPress
  | KeyUp
  | KeyDown

instance Event KeyEvent where
  type EventData KeyEvent = KeyData
  eventName KeyPress = "keypress"
  eventName KeyUp    = "keyup"
  eventName KeyDown  = "keydown"
  eventData _ e =
    KeyData <$> get e "keyCode"
            <*> get e "ctrlKey"
            <*> get e "altKey"
            <*> get e "shiftKey"
            <*> get e "metaKey"
