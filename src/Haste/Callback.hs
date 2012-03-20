{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Haste.Callback (Callback, mkCallback, ElemID, Event (..),
                       setCallback) where
import Haste.Prim
import Foreign.Ptr (Ptr)

newtype Callback = Callback (Ptr (IO ()))

foreign import ccall jsSetCB :: JSString -> JSString -> Callback -> IO Bool

-- | Turn an IO computation into a callback that can be passed to a JS
--   function.
mkCallback :: IO () -> Callback
mkCallback = Callback . mkPtr

type ElemID = String

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

-- | Set a callback on an event of an element.
--   Returns False if the given element could not be found.
setCallback :: ElemID -> Event -> IO () -> IO Bool
setCallback elemId evt f =
  jsSetCB (toJSStr elemId) (toJSStr evtName) (mkCallback f)
  where
    evtName =
      case evt of
        OnLoad      -> "onload"
        OnUnload    -> "onunload"
        OnClick     -> "onclick"
        OnDblClick  -> "ondblclick"
        OnMouseDown -> "onmousedown"
        OnMouseUp   -> "onmouseup"
        OnMouseMove -> "onmousemove"
        OnMouseOver -> "onmouseover"
        OnMouseOut  -> "onmouseout"
        OnKeyPress  -> "onkeypress"
        OnKeyUp     -> "onkeyup"
        OnKeyDown   -> "onkeydown"
        OnChange    -> "onchange"
        OnFocus     -> "onfocus"
        OnBlur      -> "onblur"
