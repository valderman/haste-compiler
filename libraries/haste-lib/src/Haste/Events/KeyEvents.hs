{-# LANGUAGE OverloadedStrings, TypeFamilies, CPP #-}
-- | Events relating to mouse keyboard input.
module Haste.Events.KeyEvents (KeyEvent (..), KeyData (..), mkKeyData) where
import Haste.Object
import Haste.JSType
import Haste.Events.Core
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Maybe

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

-- | Num instance for KeyData to enable pattern matching against numeric
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
    KeyData <$> (e # "keyCode"  >>= fmap (convert . fromJust) . asNumber)
            <*> (e # "ctrlKey"  >>= fmap fromJust . asBool)
            <*> (e # "altKey"   >>= fmap fromJust . asBool)
            <*> (e # "shiftKey" >>= fmap fromJust . asBool)
            <*> (e # "metaKey"  >>= fmap fromJust . asBool)
