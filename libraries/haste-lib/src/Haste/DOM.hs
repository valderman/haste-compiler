{-# LANGUAGE ForeignFunctionInterface #-}
module Haste.DOM (Elem (..), PropID, ElemID,
                  newElem, elemById, setProp, getProp, withElem,
                  addChild, addChildBefore, removeChild, clearChildren,
                  getChildBefore, getLastChild, getChildren, setChildren,
                  getStyle, setStyle) where
import Haste.Prim
import Control.Monad.IO.Class

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
addChild :: MonadIO m => Elem -> Elem -> m ()
addChild child parent = liftIO $ jsAppendChild child parent

-- | Insert the first element as a child into the second, before the third.
--   For instance:
-- @
--   addChildBefore childToAdd theContainer olderChild
-- @
addChildBefore :: MonadIO m => Elem -> Elem -> Elem -> m ()
addChildBefore child parent oldChild =
  liftIO $ jsAddChildBefore child parent oldChild

-- | Get the sibling before the given one, if any.
getChildBefore :: MonadIO m => Elem -> m (Maybe Elem)
getChildBefore e = liftIO $ fromPtr `fmap` jsGetChildBefore e

-- | Get the last of an element's children.
getLastChild :: MonadIO m => Elem -> m (Maybe Elem)
getLastChild e = liftIO $ fromPtr `fmap` jsGetLastChild e

-- | Get a list of all children belonging to a certain element.
getChildren :: MonadIO m => Elem -> m [Elem]
getChildren e = liftIO $ fromPtr `fmap` jsGetChildren e

-- | Clear the given element's list of children, and append all given children
--   to it.
setChildren :: MonadIO m => Elem -> [Elem] -> m ()
setChildren e ch = liftIO $ jsSetChildren e (toPtr ch)

-- | Create an element.
newElem :: MonadIO m => String -> m Elem
newElem = liftIO . jsCreateElem . toJSStr

-- | Set a property of the given element.
setProp :: MonadIO m => Elem -> PropID -> String -> m ()
setProp e prop val = liftIO $ jsSet e (toJSStr prop) (toJSStr val)

-- | Get a property of an element.
getProp :: MonadIO m => Elem -> PropID -> m String
getProp e prop = liftIO $ fromJSStr `fmap` jsGet e (toJSStr prop)

-- | Get a CSS style property of an element.
getStyle :: MonadIO m => Elem -> PropID -> m String
getStyle e prop = liftIO $ fromJSStr `fmap` jsGetStyle e (toJSStr prop)

-- | Get a CSS style property of an element.
setStyle :: MonadIO m => Elem -> PropID -> String -> m ()
setStyle e prop val = liftIO $ jsSetStyle e (toJSStr prop) (toJSStr val)

-- | Get an element by its HTML ID attribute.
elemById :: MonadIO m => ElemID -> m (Maybe Elem)
elemById eid = liftIO $ fromPtr `fmap` (jsFind $ toJSStr eid)

-- | Perform an IO action on an element.
withElem :: MonadIO m => ElemID -> (Elem -> m a) -> m a
withElem e act = do
  me' <- elemById e
  case me' of
    Just e' -> act e'
    _       -> error $ "No element with ID " ++ e ++ " could be found!"

-- | Remove all children from the given element.
clearChildren :: MonadIO m => Elem -> m ()
clearChildren = liftIO . jsClearChildren

-- | Remove the first element from the second's children.
removeChild :: MonadIO m => Elem -> Elem -> m ()
removeChild child parent = liftIO $ jsKillChild child parent
