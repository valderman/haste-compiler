{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, GADTs,
             FlexibleInstances, OverloadedStrings #-}
module Haste.Callback (setCallback, JSFun (..), mkCallback, Event (..),
                       setTimeout, Callback (..)) where
import Haste.Prim
import Haste.DOM
import Data.String

newtype JSFun a = JSFun (Ptr a)

foreign import ccall jsSetCB :: Elem -> JSString -> JSFun a -> IO Bool
foreign import ccall jsSetTimeout :: Int -> JSFun a -> IO ()

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
  OnMouseMove :: Event (IO ())
  OnMouseOver :: Event (IO ())
  OnMouseOut  :: Event (IO ())
  OnClick     :: Event (Int -> IO ())
  OnDblClick  :: Event (Int -> IO ())
  OnMouseDown :: Event (Int -> IO ())
  OnMouseUp   :: Event (Int -> IO ())
  OnKeyPress  :: Event (Int -> IO ())
  OnKeyUp     :: Event (Int -> IO ())
  OnKeyDown   :: Event (Int -> IO ())

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

-- | Set a callback for the given event.
setCallback :: Elem -> Event a -> a -> IO Bool
setCallback e evt f =
  jsSetCB e (evtName evt) (mkCallback $! f)

-- | Wrapper for window.setTimeout; execute the given computation after a delay
--   given in milliseconds.
setTimeout :: Int -> IO () -> IO ()
setTimeout delay cb =
  jsSetTimeout delay (mkCallback $! cb)
