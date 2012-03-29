{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Haste.Callback (Callback, mkCallback, ElemID, PropID, Event (..),
                       setCallback, getProp, setProp) where
import Haste.Prim
import Foreign.Ptr (Ptr)
import Control.Applicative

newtype Callback = Callback (Ptr (IO ()))

foreign import ccall jsSetCB :: JSString -> JSString -> Callback -> IO Bool
foreign import ccall jsFind :: JSString -> IO Bool
foreign import ccall jsGet :: JSString -> JSString  -> IO JSString
foreign import ccall jsSet :: JSString -> JSString -> JSString -> IO ()

-- | Turn an IO computation into a callback that can be passed to a JS
--   function.
mkCallback :: IO () -> Callback
mkCallback = Callback . mkPtr

type ElemID = String
type PropID = String

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
  jsSetCB (toJSStr elemId) (toJSStr evtName) (mkCallback $! f)
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

withElem :: ElemID -> (JSString -> IO a) -> IO a
withElem e act = do
  let e' = toJSStr e
  exists <- jsFind e'
  case exists of
    False -> error $ "No element with ID " ++ e ++ " could be found!"
    _     -> act e'

-- | Get a property from a JS object. Panics if the object doesn't exist.
getProp :: ElemID -> PropID -> IO String
getProp e p = withElem e $ \e' -> fromJSStr <$> jsGet e' (toJSStr p)

-- | Set a property in a JS object. Panics if the object doesn't exist.
setProp :: ElemID -> PropID -> String -> IO ()
setProp e p v = withElem e $ \e' -> jsSet e' (toJSStr p) (toJSStr v)
