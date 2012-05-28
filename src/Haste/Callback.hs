{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Haste.Callback (Callback (..), mkCallback, Event (..),
                       setCallback, setTimeout) where
import Haste.Prim
import Haste.DOM

newtype Callback a = Callback (Ptr a)

foreign import ccall jsSetCB :: Elem -> JSString -> Callback a -> IO Bool
foreign import ccall jsSetTimeout :: Int -> Callback a -> IO ()

-- | Turn a computation into a callback that can be passed to a JS
--   function.
mkCallback :: a -> Callback a
mkCallback = Callback . toPtr

data Event
  = OnLoad
  | OnUnload
  | OnClick
  | OnDblClick
  | OnMouseDown
  | OnMouseUp
  | OnMouseMove
  | OnMouseOver
  | OnMouseOut
  | OnKeyPress
  | OnKeyUp
  | OnKeyDown
  | OnChange
  | OnFocus
  | OnBlur

-- | Wrapper for window.setTimeout; execute the given computation after a delay
--   given in milliseconds.
setTimeout :: Int -> IO () -> IO ()
setTimeout delay cb =
  jsSetTimeout delay (mkCallback $! cb)

-- | Set a callback on an event of an element.
--   Returns False if the given element could not be found.
setCallback :: Elem -> Event -> IO () -> IO Bool
setCallback e evt f =
  jsSetCB e (toJSStr evtName) (mkCallback $! f)
  where
    evtName =
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
