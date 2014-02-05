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

data Event m a where
  OnLoad      :: Event m (m ())
  OnUnload    :: Event m (m ())
  OnChange    :: Event m (m ())
  OnFocus     :: Event m (m ())
  OnBlur      :: Event m (m ())
  OnMouseMove :: Event m ((Int, Int) -> m ())
  OnMouseOver :: Event m ((Int, Int) -> m ())
  OnMouseOut  :: Event m (m ())
  OnClick     :: Event m (Int -> (Int, Int) -> m ())
  OnDblClick  :: Event m (Int -> (Int, Int) -> m ())
  OnMouseDown :: Event m (Int -> (Int, Int) -> m ())
  OnMouseUp   :: Event m (Int -> (Int, Int) -> m ())
  OnKeyPress  :: Event m (Int -> m ())
  OnKeyUp     :: Event m (Int -> m ())
  OnKeyDown   :: Event m (Int -> m ())

asEvtTypeOf :: Event m a -> a -> a
asEvtTypeOf _ = id

instance Eq (Event m a) where
  a == b = evtName a == (evtName b :: String)

instance Ord (Event m a) where
  compare a b = compare (evtName a) (evtName b :: String)

-- | The name of a given event.
evtName :: IsString s => Event m a -> s
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
onEvent :: MonadIO m => Elem -> Event m a -> a -> m Bool
onEvent = setCallback

-- | Friendlier name for @setCallback'@.
onEvent' :: (ToConcurrent a, MonadIO m) => Elem -> Event m a -> Async a -> m Bool
onEvent' = setCallback'

-- | Set a callback for the given event.
setCallback :: MonadIO m => Elem -> Event m a -> a -> m Bool
setCallback e evt f =
  liftIO $ jsSetCB e (evtName evt) (mkCallback $! f)

-- | Like @setCallback@, but takes a callback in the CIO monad instead of IO.
setCallback' :: (ToConcurrent a, MonadIO m)
             => Elem
             -> Event m a
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
