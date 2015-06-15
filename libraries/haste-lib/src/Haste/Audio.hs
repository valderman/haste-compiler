{-# LANGUAGE OverloadedStrings, CPP #-}
-- | High-ish level bindings to the HTML5 audio tag and JS API.
module Haste.Audio (
    module Events,
    Audio, AudioSettings (..), AudioType (..), AudioSource (..),
    AudioPreload (..), AudioState (..), Seek (..),
    defaultAudioSettings,
    mkSource, newAudio, setSource,
    getState,
    setMute, isMute, toggleMute,
    setLooping, isLooping, toggleLooping,
    getVolume, setVolume, modVolume,
    play, pause, stop, togglePlaying,
    seek, getDuration, getCurrentTime
  ) where
import Haste.Audio.Events as Events
import Haste.DOM.JSString
import Haste.Foreign
import Haste.JSType
import Haste.Prim
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad
import Control.Monad.IO.Class
import Data.String

-- | Represents an audio player.
data Audio = Audio Elem

instance IsElem Audio where
  elemOf (Audio e) = e
  fromElem e = do
    tn <- getProp e "tagName"
    return $ case tn of
      "AUDIO" -> Just $ Audio e
      _       -> Nothing

data AudioState = Playing | Paused | Ended
  deriving (Show, Eq)
data AudioType = MP3 | OGG | WAV
  deriving (Show, Eq)
data AudioSource = AudioSource !AudioType !JSString
  deriving (Show, Eq)
data AudioPreload = None | Metadata | Auto
  deriving Eq
data Seek = Start | End | Seconds Double
  deriving Eq

instance JSType AudioPreload where
  toJSString None     = "none"
  toJSString Metadata = "metadata"
  toJSString Auto     = "auto"
  fromJSString "none"     = Just None
  fromJSString "metadata" = Just Metadata
  fromJSString "auto"     = Just Auto
  fromJSString _          = Nothing

data AudioSettings = AudioSettings {
    -- | Show controls?
    --   Default: False
    audioControls :: !Bool,
    -- | Immediately start playing?
    --   Default: False
    audioAutoplay :: !Bool,
    -- | Initially looping?
    --   Default: False
    audioLooping  :: !Bool,
    -- | How much audio to preload.
    --   Default: Auto
    audioPreload  :: !AudioPreload,
    -- | Initially muted?
    --   Default: False
    audioMuted    :: !Bool,
    -- | Initial volume
    --   Default: 0
    audioVolume   :: !Double
  }

defaultAudioSettings :: AudioSettings
defaultAudioSettings = AudioSettings {
    audioControls = False,
    audioAutoplay = False,
    audioLooping = False,
    audioPreload = Auto,
    audioMuted = False,
    audioVolume = 0
  }

-- | Create an audio source with automatically detected media type, based on
--   the given URL's file extension.
--   Returns Nothing if the given URL has an unrecognized media type.
mkSource :: JSString -> Maybe AudioSource
mkSource url =
  case take 3 $ reverse $ fromJSStr url of
    "3pm" -> Just $ AudioSource MP3 url
    "ggo" -> Just $ AudioSource OGG url
    "vaw" -> Just $ AudioSource WAV url
    _     -> Nothing

instance IsString AudioSource where
  fromString s =
    case mkSource $ Data.String.fromString s of
      Just src -> src
      _        -> error $ "Not a valid audio source: " ++ s

mimeStr :: AudioType -> JSString
mimeStr MP3 = "audio/mpeg"
mimeStr OGG = "audio/ogg"
mimeStr WAV = "audio/wav"

-- | Create a new audio element.
newAudio :: MonadIO m => AudioSettings -> [AudioSource] -> m Audio
newAudio cfg sources = liftIO $ do
  srcs <- forM sources $ \(AudioSource t url) -> do
    newElem "source" `with` ["type" =: mimeStr t, "src" =: toJSString url]
  Audio <$> newElem "audio" `with` [
      "controls" =: falseAsEmpty (audioControls cfg),
      "autoplay" =: falseAsEmpty (audioAutoplay cfg),
      "loop"     =: falseAsEmpty (audioLooping cfg),
      "muted"    =: falseAsEmpty (audioMuted cfg),
      "volume"   =: toJSString (audioVolume cfg),
      "preload"  =: toJSString (audioPreload cfg),
      children srcs
    ]

-- | Returns "true" or "", depending on the given boolean.
falseAsEmpty :: Bool -> JSString
falseAsEmpty True = "true"
falseAsEmpty _    = ""

-- | (Un)mute the given audio object.
setMute :: MonadIO m => Audio -> Bool -> m ()
setMute (Audio e) = setAttr e "muted" . falseAsEmpty

-- | Is the given audio object muted?
isMute :: MonadIO m => Audio -> m Bool
isMute (Audio e) = liftIO $ maybe False id . fromJSString <$> getProp e "muted"

-- | Mute/unmute.
toggleMute :: MonadIO m => Audio -> m ()
toggleMute a = isMute a >>= setMute a . not

-- | Set whether the given sound should loop upon completion or not.
setLooping :: MonadIO m => Audio -> Bool -> m ()
setLooping (Audio e) = setAttr e "loop" . falseAsEmpty

-- | Is the given audio object looping?
isLooping :: MonadIO m => Audio -> m Bool
isLooping (Audio e) =
  liftIO $ maybe False id . fromJSString <$> getProp e "looping"

-- | Toggle looping on/off.
toggleLooping :: MonadIO m => Audio -> m ()
toggleLooping a = isLooping a >>= setLooping a . not

-- | Starts playing audio from the given element.
play :: MonadIO m => Audio -> m ()
play a@(Audio e) = do
    st <- getState a
    when (st == Ended) $ seek a Start
    liftIO $ play' e
  where
    play' :: Elem -> IO ()
    play' = ffi "(function(x){x.play();})"

-- | Get the current state of the given audio object.
getState :: MonadIO m => Audio -> m AudioState
getState (Audio e) = liftIO $ do
  ended <- maybe False id . fromJSString <$> getProp e "ended"
  if ended
    then return Ended
    else maybe Playing paused . fromJSString <$> getProp e "paused"
  where
    paused True = Paused
    paused _    = Playing

-- | Pause the given audio element.
pause :: MonadIO m => Audio -> m ()
pause (Audio e) = liftIO $ pause' e

pause' :: Elem -> IO ()
pause' = ffi "(function(x){x.pause();})"

-- | If playing, stop. Otherwise, start playing.
togglePlaying :: MonadIO m => Audio -> m ()
togglePlaying a = do
  st <- getState a
  case st of
    Playing    -> pause a
    Ended      -> seek a Start >> play a
    Paused     -> play a

-- | Stop playing a track, and seek back to its beginning.
stop :: MonadIO m => Audio -> m ()
stop a = pause a >> seek a Start

-- | Get the volume for the given audio element as a value between 0 and 1.
getVolume :: MonadIO m => Audio -> m Double
getVolume (Audio e) = liftIO $ maybe 0 id . fromJSString <$> getProp e "volume"

-- | Set the volume for the given audio element. The value will be clamped to
--   [0, 1].
setVolume :: MonadIO m => Audio -> Double -> m ()
setVolume (Audio e) = setProp e "volume" . toJSString . clamp

-- | Modify the volume for the given audio element. The resulting volume will
--   be clamped to [0, 1].
modVolume :: MonadIO m => Audio -> Double -> m ()
modVolume a diff = getVolume a >>= setVolume a . (+ diff)

-- | Clamp a value to [0, 1].
clamp :: Double -> Double
clamp = max 0 . min 1

-- | Seek to the specified time.
seek :: MonadIO m => Audio -> Seek -> m ()
seek a@(Audio e) st = liftIO $ do
    case st of
      Start     -> seek' e 0
      End       -> getDuration a >>= seek' e
      Seconds s -> seek' e s
  where
    seek' :: Elem -> Double -> IO ()
    seek' = ffi "(function(e,t) {e.currentTime = t;})"

-- | Get the duration of the loaded sound, in seconds.
getDuration :: MonadIO m => Audio -> m Double
getDuration (Audio e) = do
  dur <- getProp e "duration"
  case fromJSString dur of
    Just d -> return d
    _      -> return 0

-- | Get the current play time of the loaded sound, in seconds.
getCurrentTime :: MonadIO m => Audio -> m Double
getCurrentTime (Audio e) = do
  dur <- getProp e "currentTime"
  case fromJSString dur of
    Just d -> return d
    _      -> return 0

-- | Set the source of the given audio element.
setSource :: MonadIO m => Audio -> AudioSource -> m ()
setSource (Audio e) (AudioSource _ url) = setProp e "src" (toJSString url)
