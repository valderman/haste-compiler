{-# LANGUAGE GADTs #-}
-- | DOM manipulation functions using 'String' for string representation.
module Haste.DOM (
    module Core,
    IsElem (..), Elem, PropID, ElemID, QuerySelector, ElemClass,
    AttrName, AttrValue,
    style, attr, (=:),
    newElem, newTextElem,
    elemById, elemsByQS, elemsByClass,
    setProp, getProp, setAttr, getAttr, J.getValue,
    withElem , withElems, withElemsQS, mapQS, mapQS_,
    getStyle, setStyle,
    J.getFileData, getFileName,
    setClass, toggleClass, hasClass
  ) where
import qualified Haste.DOM.JSString as J
import qualified Haste.DOM.Core as Core
  hiding (Elem (..), AttrName (..))
import Haste.DOM.Core
import Haste.Prim (fromJSStr, toJSStr)
import Control.Monad.IO.Class

type PropID = String
type ElemID = String
type QuerySelector = String
type ElemClass = String
type AttrValue = String

-- | Create a style attribute name.
style :: String -> AttrName
style = StyleName . toJSStr

-- | Create an HTML attribute name.
attr :: String -> AttrName
attr = AttrName . toJSStr

-- | Create an 'Attribute'.
(=:) :: AttrName -> AttrValue -> Attribute
name =: val = attribute name (toJSStr val)

-- | Create an element.
newElem :: MonadIO m => String -> m Elem
newElem = J.newElem . toJSStr

-- | Create a text node.
newTextElem :: MonadIO m => String -> m Elem
newTextElem = J.newTextElem . toJSStr

-- | Set a property of the given element.
setProp :: (IsElem e, MonadIO m) => e -> PropID -> String -> m ()
setProp e prop val = J.setProp e (toJSStr prop) (toJSStr val)

-- | Set an attribute of the given element.
setAttr :: (IsElem e, MonadIO m) => e -> PropID -> String -> m ()
setAttr e prop val = J.setAttr e (toJSStr prop) (toJSStr val)

-- | Get a property of an element.
getProp :: (IsElem e, MonadIO m) => e -> PropID -> m String
getProp e prop = J.getProp e (toJSStr prop) >>= return . fromJSStr

-- | Get an attribute of an element.
getAttr :: (IsElem e, MonadIO m) => e -> PropID -> m String
getAttr e prop = J.getAttr e (toJSStr prop) >>= return . fromJSStr

-- | Get a CSS style property of an element.
getStyle :: (IsElem e, MonadIO m) => e -> PropID -> m String
getStyle e prop = J.getStyle e (toJSStr prop) >>= return . fromJSStr

-- | Set a CSS style property on an element.
setStyle :: (IsElem e, MonadIO m) => e -> PropID -> String -> m ()
setStyle e prop val = J.setStyle e (toJSStr prop) (toJSStr val)

-- | Get an element by its HTML ID attribute.
elemById :: MonadIO m => ElemID -> m (Maybe Elem)
elemById = J.elemById . toJSStr

-- | Get all elements of the given class.
elemsByClass :: MonadIO m => ElemClass -> m [Elem]
elemsByClass = J.elemsByClass . toJSStr

-- | Get all children elements matching a query selector.
elemsByQS :: MonadIO m => Elem -> QuerySelector -> m [Elem]
elemsByQS el = J.elemsByQS el . toJSStr

-- | Perform an IO action on an element.
withElem :: MonadIO m => ElemID -> (Elem -> m a) -> m a
withElem = J.withElem . toJSStr

-- | Perform an IO action over several elements. Throws an error if some of the
--   elements are not found.
withElems :: MonadIO m => [ElemID] -> ([Elem] -> m a) -> m a
withElems = J.withElems . map toJSStr

-- | Perform an IO action over the a list of elements matching a query
--   selector.
withElemsQS :: (IsElem e, MonadIO m)
            => e
            -> QuerySelector
            -> ([Elem] -> m a)
            -> m a
withElemsQS el = J.withElemsQS el . toJSStr

-- | Map an IO computation over the list of elements matching a query selector.
mapQS :: (IsElem e, MonadIO m)
      => e
      -> QuerySelector
      -> (Elem -> m a)
      -> m [a]
mapQS el = J.mapQS el . toJSStr

-- | Like @mapQS@ but returns no value.
mapQS_ :: (IsElem e, MonadIO m)
       => e
       -> QuerySelector
       -> (Elem -> m a)
       -> m ()
mapQS_ el = J.mapQS_ el . toJSStr

-- | Get the name of the currently selected file from a file input element.
--   Any directory information is stripped, and only the actual file name is
--   returned, as the directory information is useless (and faked) anyway.
getFileName :: (IsElem e, MonadIO m) => e -> m String
getFileName e = J.getFileName e >>= return . fromJSStr

-- | Add or remove a class from an element's class list.
setClass :: (IsElem e, MonadIO m) => e -> String -> Bool -> m ()
setClass e sel = J.setClass e (toJSStr sel)

-- | Toggle the existence of a class within an elements class list.
toggleClass :: (IsElem e, MonadIO m) => e -> String -> m ()
toggleClass e = J.toggleClass e . toJSStr

-- | Does the given element have a particular class?
hasClass :: (IsElem e, MonadIO m) => e -> String -> m Bool
hasClass e = J.hasClass e . toJSStr
