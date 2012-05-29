{-# LANGUAGE ForeignFunctionInterface #-}
module Haste.DOM (Elem (..), PropID, ElemID,
                  newElem, elemById, setProp, getProp, withElem,
                  addChild, addChildBefore, removeChild, clearChildren,
                  getChildBefore, getLastChild, getChildren, setChildren,
                  getStyle, setStyle) where
import Haste.Prim

newtype Elem = Elem JSAny
type PropID = String
type ElemID = String

foreign import ccall jsGet :: Elem -> JSString -> IO JSString
foreign import ccall jsSet :: Elem -> JSString -> JSString -> IO ()
foreign import ccall jsGetStyle :: Elem -> JSString -> IO JSString
foreign import ccall jsSetStyle :: Elem -> JSString -> JSString -> IO ()
foreign import ccall jsFind :: JSString -> IO (Ptr (Maybe Elem))
foreign import ccall jsCreateElem :: JSString -> IO Elem
foreign import ccall jsAppendChild :: Elem -> Elem -> IO ()
foreign import ccall jsGetLastChild :: Elem -> IO (Ptr (Maybe Elem))
foreign import ccall jsGetChildren :: Elem -> IO (Ptr [Elem])
foreign import ccall jsSetChildren :: Elem -> Ptr [Elem] -> IO ()
foreign import ccall jsAddChildBefore :: Elem -> Elem -> Elem -> IO ()
foreign import ccall jsGetChildBefore :: Elem -> IO (Ptr (Maybe Elem))
foreign import ccall jsKillChild :: Elem -> Elem -> IO ()
foreign import ccall jsClearChildren :: Elem -> IO ()

-- | Append the first element as a child of the second element.
addChild :: Elem -> Elem -> IO ()
addChild = jsAppendChild

-- | Insert the first element as a child into the second, before the third.
--   For instance:
-- @
--   addChildBefore childToAdd theContainer olderChild
-- @
addChildBefore :: Elem -> Elem -> Elem -> IO ()
addChildBefore = jsAddChildBefore

-- | Get the sibling before the given one, if any.
getChildBefore :: Elem -> IO (Maybe Elem)
getChildBefore e = fromPtr `fmap` jsGetChildBefore e

-- | Get the last of an element's children.
getLastChild :: Elem -> IO (Maybe Elem)
getLastChild e = fromPtr `fmap` jsGetLastChild e

-- | Get a list of all children belonging to a certain element.
getChildren :: Elem -> IO [Elem]
getChildren e = fromPtr `fmap` jsGetChildren e

-- | Clear the given element's list of children, and append all given children
--   to it.
setChildren :: Elem -> [Elem] -> IO ()
setChildren e ch = jsSetChildren e (toPtr ch)

-- | Create an element.
newElem :: String -> IO Elem
newElem = jsCreateElem . toJSStr

-- | Set a property of the given element.
setProp :: Elem -> PropID -> String -> IO ()
setProp e prop val = jsSet e (toJSStr prop) (toJSStr val)

-- | Get a property of an element.
getProp :: Elem -> PropID -> IO String
getProp e prop = fromJSStr `fmap` jsGet e (toJSStr prop)

-- | Get a CSS style property of an element.
getStyle :: Elem -> PropID -> IO String
getStyle e prop = fromJSStr `fmap` jsGetStyle e (toJSStr prop)

-- | Get a CSS style property of an element.
setStyle :: Elem -> PropID -> String -> IO ()
setStyle e prop val = jsSetStyle e (toJSStr prop) (toJSStr val)

-- | Get an element by its HTML ID attribute.
elemById :: String -> IO (Maybe Elem)
elemById eid = do
  fromPtr `fmap` (jsFind $ toJSStr eid)

-- | Perform an IO action on an element.
withElem :: ElemID -> (Elem -> IO a) -> IO a
withElem e act = do
  me' <- elemById e
  case me' of
    Just e' -> act e'
    _       -> error $ "No element with ID " ++ e ++ " could be found!"

-- | Remove all children from the given element.
clearChildren :: Elem -> IO ()
clearChildren = jsClearChildren

-- | Remove the first element from the second's children.
removeChild :: Elem -> Elem -> IO ()
removeChild = jsKillChild
