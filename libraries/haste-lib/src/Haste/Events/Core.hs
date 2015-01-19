{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts #-}
-- | Basic framework for event handling.
module Haste.Events.Core (
    Event (..), MonadEvent (..),
    HandlerInfo,
    unregisterHandler, onEvent, preventDefault
  ) where
import Haste.Prim
import Haste.DOM.Core
import Haste.Foreign
import Haste.Object
import Control.Monad.IO.Class
import Data.IORef
import System.IO.Unsafe

-- | Any monad in which we're able to handle events.
class MonadIO m => MonadEvent m where
  mkHandler :: (a -> m ()) -> m (a -> IO ())

instance MonadEvent IO where
  mkHandler = return

-- | Any type that describes an event.
class Event evt where
  -- | The type of data to pass to handlers for this event.
  type EventData evt

  -- | The name of this event, as expected by the DOM.
  eventName :: evt -> JSString

  -- | Construct event data from the event identifier and the JS event object.
  eventData :: evt -> JSObj -> IO (EventData evt)

-- | Information about an event handler.
data HandlerInfo = HandlerInfo {
    -- | Name of the handler's event.
    handlerEvent :: JSString,
    -- | Element the handler is set on.
    handlerElem  :: Elem,
    -- | Handle to handler function.
    handlerFun   :: Unpacked
  }

-- | Unregister an event handler.
unregisterHandler :: HandlerInfo -> IO ()
unregisterHandler (HandlerInfo ev el f) = unregEvt el ev f

-- | Reference to the event currently being handled.
evtRef :: IORef JSObj
evtRef = unsafePerformIO $ newIORef Nothing

{-# INLINE setEvtRef #-}
setEvtRef :: JSObj -> IO ()
setEvtRef = writeIORef evtRef

-- | Prevent the event being handled from resolving normally.
--   Does nothing if called outside an event handler.
preventDefault :: IO ()
preventDefault = readIORef evtRef >>= go
  where
    {-# NOINLINE go #-}
    go :: JSObj -> IO ()
    go = ffi "(function(e){if(e){e.preventDefault();}})"

-- | Set an event handler on a DOM element.
onEvent :: (MonadEvent m, IsElem el, Event evt)
        => el            -- ^ Element to set handler on.
        -> evt           -- ^ Event to handle.
        -> (EventData evt -> m ()) -- ^ Event handler.
        -> m HandlerInfo -- ^ Information about the handler.
onEvent el evt f = do
  f' <- mkHandler $ \o -> prepareEvent o >>= f
  hdl <- liftIO $ setEvt e name f'
  return $ HandlerInfo {
      handlerEvent = name,
      handlerElem  = e,
      handlerFun   = hdl
    }
  where
    name = eventName evt
    e    = elemOf el
    prepareEvent o = liftIO $ do
      let o' = Just o
      setEvtRef o'
      eventData evt o'

-- | Set an event handler on an element, returning a reference to the handler
--   exactly as seen by @addEventListener@. We can't reuse the reference to
--   the Haskell function as the FFI does some marshalling to functions,
--   meaning that the same function marshalled twice won't be reference equal
--   to each other.
{-# NOINLINE setEvt #-}
setEvt :: Elem -> JSString -> (JSAny -> IO ()) -> IO Unpacked
setEvt = ffi "(function(e,name,f){e.addEventListener(name,f,false);\
             \return [f];})"

-- | Unregister an event.
--   Note @f[0]@ and corresponding @[f]@ in 'setEvt'; this is a workaround for
--   a bug causing functions being packed into anything to be accidentally
--   called. Remove when properly fixed.
{-# NOINLINE unregEvt #-}
unregEvt :: Elem -> JSString -> Unpacked -> IO ()
unregEvt = ffi "(function(e,name,f){e.removeEventListener(name,f[0]);})"
