{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, GADTs,
             FlexibleInstances, OverloadedStrings, CPP #-}
module Haste.Callback (
    setCallback, setCallback', JSFun (..), mkCallback, Event (..),
    setTimeout, setTimeout', Callback (..), onEvent, onEvent'
  ) where
import Haste.Prim
import Haste.DOM
import Haste.Concurrent.Monad
import Data.String
import Control.Monad.IO.Class

newtype JSFun a = JSFun (Ptr a)

#ifdef __HASTE__
foreign import ccall jsSetCB :: Elem -> JSString -> JSFun a -> IO Bool
foreign import ccall jsSetTimeout :: Int -> JSFun a -> IO ()
#else
jsSetCB :: Elem -> JSString -> JSFun a -> IO Bool
jsSetCB = error "Tried to use jsSetCB on server side!"
jsSetTimeout :: Int -> JSFun a -> IO ()
jsSetTimeout = error "Tried to use jsSetTimeout on server side!"
#endif

-- | Turn a computation into a callback that can be passed to a JS
--   function.
mkCallback :: a -> JSFun a
mkCallback = JSFun . toPtr

class Callback a where
  constCallback :: IO () -> a

instance Callback (IO ()) where
  constCallback = id

instance Callback (a -> IO ()) where
  constCallback = const

data Event a where
  OnLoad      :: Event (IO ())
  OnUnload    :: Event (IO ())
  OnChange    :: Event (IO ())
  OnFocus     :: Event (IO ())
  OnBlur      :: Event (IO ())
  OnMouseMove :: Event ((Int, Int) -> IO ())
  OnMouseOver :: Event ((Int, Int) -> IO ())
  OnMouseOut  :: Event (IO ())
  OnClick     :: Event (Int -> (Int, Int) -> IO ())
  OnDblClick  :: Event (Int -> (Int, Int) -> IO ())
  OnMouseDown :: Event (Int -> (Int, Int) -> IO ())
  OnMouseUp   :: Event (Int -> (Int, Int) -> IO ())
  OnKeyPress  :: Event (Int -> IO ())
  OnKeyUp     :: Event (Int -> IO ())
  OnKeyDown   :: Event (Int -> IO ())

asEvtTypeOf :: Event a -> a -> a
asEvtTypeOf _ = id

instance Eq (Event a) where
  a == b = evtName a == (evtName b :: String)

instance Ord (Event a) where
  compare a b = compare (evtName a) (evtName b :: String)

-- | The name of a given event.
evtName :: IsString s => Event a -> s
evtName evt =
  case evt of
    OnLoad      -> "load"
    OnUnload    -> "unload"
    OnClick     -> "click"
    OnDblClick  -> "dblclick"
    OnMouseDown -> "mousedown"
    OnMouseUp   -> "mouseup"
    OnMouseMove -> "mousemove"
    OnMouseOver -> "mouseover"
    OnMouseOut  -> "mouseout"
    OnKeyPress  -> "keypress"
    OnKeyUp     -> "keyup"
    OnKeyDown   -> "keydown"
    OnChange    -> "change"
    OnFocus     -> "focus"
    OnBlur      -> "blur"

-- | Friendlier name for @setCallback@.
onEvent :: MonadIO m => Elem -> Event a -> a -> m Bool
onEvent = setCallback

-- | Friendlier name for @setCallback'@.
onEvent' :: (ToConcurrent a, MonadIO m) => Elem -> Event a -> Async a -> m Bool
onEvent' = setCallback'

-- | Set a callback for the given event.
setCallback :: MonadIO m => Elem -> Event a -> a -> m Bool
setCallback e evt f =
  liftIO $ jsSetCB e (evtName evt) (mkCallback $! f)

-- | Like @setCallback@, but takes a callback in the CIO monad instead of IO.
setCallback' :: (ToConcurrent a, MonadIO m)
             => Elem
             -> Event a
             -> Async a
             -> m Bool
setCallback' e evt f =
    liftIO $ jsSetCB e (evtName evt) (mkCallback $! f')
  where
    f' = asEvtTypeOf evt (async f)

-- | Wrapper for window.setTimeout; execute the given computation after a delay
--   given in milliseconds.
setTimeout :: MonadIO m => Int -> IO () -> m ()
setTimeout delay cb =
  liftIO $ jsSetTimeout delay (mkCallback $! cb)

-- | Like 'setTimeout', but takes a callback in the CIO monad instead of IO.
setTimeout' :: MonadIO m => Int -> CIO () -> m ()
setTimeout' delay cb =
  liftIO $ jsSetTimeout delay (mkCallback $! concurrent cb)
