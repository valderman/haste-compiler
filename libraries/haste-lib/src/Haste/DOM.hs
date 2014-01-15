{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, CPP #-}
module Haste.DOM (
    Elem (..), PropID, ElemID,
    newElem, newTextElem,
    elemById, setProp, getProp, setAttr, getAttr, setProp',
    getProp', getValue, withElem , withElems, addChild,
    addChildBefore, removeChild, clearChildren , getChildBefore,
    getLastChild, getChildren, setChildren , getStyle, setStyle,
    getStyle', setStyle'
  ) where
import Haste.Prim
import Haste.JSType
import Data.Maybe (isNothing, fromJust)
import Control.Monad.IO.Class

newtype Elem = Elem JSAny
type PropID = String
type ElemID = String

#ifdef __HASTE__
foreign import ccall jsGet :: Elem -> JSString -> IO JSString
foreign import ccall jsSet :: Elem -> JSString -> JSString -> IO ()
foreign import ccall jsGetAttr :: Elem -> JSString -> IO JSString
foreign import ccall jsSetAttr :: Elem -> JSString -> JSString -> IO ()
foreign import ccall jsGetStyle :: Elem -> JSString -> IO JSString
foreign import ccall jsSetStyle :: Elem -> JSString -> JSString -> IO ()
foreign import ccall jsFind :: JSString -> IO (Ptr (Maybe Elem))
foreign import ccall jsCreateElem :: JSString -> IO Elem
foreign import ccall jsCreateTextNode :: JSString -> IO Elem
foreign import ccall jsAppendChild :: Elem -> Elem -> IO ()
foreign import ccall jsGetLastChild :: Elem -> IO (Ptr (Maybe Elem))
foreign import ccall jsGetChildren :: Elem -> IO (Ptr [Elem])
foreign import ccall jsSetChildren :: Elem -> Ptr [Elem] -> IO ()
foreign import ccall jsAddChildBefore :: Elem -> Elem -> Elem -> IO ()
foreign import ccall jsGetChildBefore :: Elem -> IO (Ptr (Maybe Elem))
foreign import ccall jsKillChild :: Elem -> Elem -> IO ()
foreign import ccall jsClearChildren :: Elem -> IO ()
#else
jsGet = error "Tried to use jsGet on server side!"
jsSet = error "Tried to use jsSet on server side!"
jsGetAttr = error "Tried to use jsGetAttr on server side!"
jsSetAttr = error "Tried to use jsSetAttr on server side!"
jsGetStyle = error "Tried to use jsGetStyle on server side!"
jsSetStyle = error "Tried to use jsSetStyle on server side!"
jsFind = error "Tried to use jsFind on server side!"
jsCreateElem = error "Tried to use jsCreateElem on server side!"
jsCreateTextNode = error "Tried to use jsCreateTextNode on server side!"
jsAppendChild = error "Tried to use jsAppendChild on server side!"
jsGetLastChild = error "Tried to use jsGetLastChild on server side!"
jsGetChildren = error "Tried to use jsGetChildren on server side!"
jsSetChildren = error "Tried to use jsSetChildren on server side!"
jsAddChildBefore = error "Tried to use jsAddChildBefore on server side!"
jsGetChildBefore = error "Tried to use jsGetChildBefore on server side!"
jsKillChild = error "Tried to use jsKillChild on server side!"
jsClearChildren = error "Tried to use jsClearChildren on server side!"
#endif

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

-- | Create a text node.
newTextElem :: MonadIO m => String -> m Elem
newTextElem = liftIO . jsCreateTextNode . toJSStr

-- | Set a property of the given element.
setProp :: MonadIO m => Elem -> PropID -> String -> m ()
setProp e prop val = liftIO $ jsSet e (toJSStr prop) (toJSStr val)

-- | Set a property of the given element, JSString edition.
setProp' :: MonadIO m => Elem -> JSString -> JSString -> m ()
setProp' e prop val = liftIO $ jsSet e prop val

-- | Set an attribute of the given element.
setAttr :: MonadIO m => Elem -> PropID -> String -> m ()
setAttr e prop val = liftIO $ jsSetAttr e (toJSStr prop) (toJSStr val)

-- | Get the value property of an element; a handy shortcut.
getValue :: (MonadIO m, JSType a) => Elem -> m (Maybe a)
getValue e = liftIO $ fromJSString `fmap` jsGet e "value"

-- | Get a property of an element.
getProp :: MonadIO m => Elem -> PropID -> m String
getProp e prop = liftIO $ fromJSStr `fmap` jsGet e (toJSStr prop)

-- | Get a property of an element, JSString edition.
getProp' :: MonadIO m => Elem -> JSString -> m JSString
getProp' e prop = liftIO $ jsGet e prop

-- | Get an attribute of an element.
getAttr :: MonadIO m => Elem -> PropID -> m String
getAttr e prop = liftIO $ fromJSStr `fmap` jsGetAttr e (toJSStr prop)

-- | Get a CSS style property of an element.
getStyle :: MonadIO m => Elem -> PropID -> m String
getStyle e prop = liftIO $ fromJSStr `fmap` jsGetStyle e (toJSStr prop)

-- | Get a CSS style property of an element, JSString style.
getStyle' :: MonadIO m => Elem -> JSString -> m JSString
getStyle' e prop = liftIO $ jsGetStyle e prop

-- | Set a CSS style property on an element.
setStyle :: MonadIO m => Elem -> PropID -> String -> m ()
setStyle e prop val = liftIO $ jsSetStyle e (toJSStr prop) (toJSStr val)

-- | Set a CSS style property on an element, JSString style.
setStyle' :: MonadIO m => Elem -> JSString -> JSString -> m ()
setStyle' e prop val = liftIO $ jsSetStyle e prop val

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

-- | Perform an IO action over several elements. Throws an error if some of the
--   elements are not found.
withElems :: MonadIO m => [ElemID] -> ([Elem] -> m a) -> m a
withElems es act = do
    mes <- mapM elemById es
    if any isNothing mes
      then error $ "Elements with the following IDs could not be found: "
                 ++ show (findElems es mes)
      else act $ map fromJust mes
  where
    findElems (i:is) (Nothing:mes) = i : findElems is mes
    findElems (_:is) (_:mes)       = findElems is mes
    findElems _ _                  = []

-- | Remove all children from the given element.
clearChildren :: MonadIO m => Elem -> m ()
clearChildren = liftIO . jsClearChildren

-- | Remove the first element from the second's children.
removeChild :: MonadIO m => Elem -> Elem -> m ()
removeChild child parent = liftIO $ jsKillChild child parent
