-- | DOM manipulation functions using 'String' for string representation.
module Haste.DOM (
    IsElem (..), Elem (..), PropID, ElemID, QuerySelector, ElemClass,
    newElem, newTextElem,
    elemById, elemsByQS, elemsByClass,
    setProp, getProp, setAttr, getAttr, J.getValue,
    withElem , withElems, withElemsQS, mapQS, mapQS_,
    J.addChild, J.addChildBefore, J.removeChild, J.clearChildren,
    J.getChildBefore, J.getFirstChild, J.getLastChild, J.getChildren,
    J.setChildren,
    getStyle, setStyle,
    J.getFileData, getFileName,
    setClass, toggleClass, hasClass,
    J.click, J.focus, J.blur,
    J.document, J.documentBody
  ) where
import qualified Haste.DOM.JSString as J
import Haste.DOM.JSString (Elem, IsElem (..))
import Haste.Prim (fromJSStr, toJSStr)
import Control.Monad.IO.Class

type PropID = String
type ElemID = String
type QuerySelector = String
type ElemClass = String

-- | Create an element.
newElem :: (Functor m, MonadIO m) => String -> m Elem
newElem = J.newElem . toJSStr

-- | Create a text node.
newTextElem :: (Functor m, MonadIO m) => String -> m Elem
newTextElem = J.newTextElem . toJSStr

-- | Set a property of the given element.
setProp :: (IsElem e, Functor m, MonadIO m) => e -> PropID -> String -> m ()
setProp e prop val = J.setProp e (toJSStr prop) (toJSStr val)

-- | Set an attribute of the given element.
setAttr :: (IsElem e, Functor m, MonadIO m) => e -> PropID -> String -> m ()
setAttr e prop val = J.setAttr e (toJSStr prop) (toJSStr val)

-- | Get a property of an element.
getProp :: (IsElem e, Functor m, MonadIO m) => e -> PropID -> m String
getProp e prop = fromJSStr `fmap` J.getProp e (toJSStr prop)

-- | Get an attribute of an element.
getAttr :: (IsElem e, Functor m, MonadIO m) => e -> PropID -> m String
getAttr e prop = fromJSStr `fmap` J.getAttr e (toJSStr prop)

-- | Get a CSS style property of an element.
getStyle :: (IsElem e, Functor m, MonadIO m) => e -> PropID -> m String
getStyle e prop = fromJSStr `fmap` J.getStyle e (toJSStr prop)

-- | Set a CSS style property on an element.
setStyle :: (IsElem e, Functor m, MonadIO m) => e -> PropID -> String -> m ()
setStyle e prop val = J.setStyle e (toJSStr prop) (toJSStr val)

-- | Get an element by its HTML ID attribute.
elemById :: (Functor m, MonadIO m) => ElemID -> m (Maybe Elem)
elemById = J.elemById . toJSStr

-- | Get all elements of the given class.
elemsByClass :: (Functor m, MonadIO m) => ElemClass -> m [Elem]
elemsByClass = J.elemsByClass . toJSStr

-- | Get all children elements matching a query selector.
elemsByQS :: (Functor m, MonadIO m) => Elem -> QuerySelector -> m [Elem]
elemsByQS el = J.elemsByQS el . toJSStr

-- | Perform an IO action on an element.
withElem :: (Functor m, MonadIO m) => ElemID -> (Elem -> m a) -> m a
withElem = J.withElem . toJSStr

-- | Perform an IO action over several elements. Throws an error if some of the
--   elements are not found.
withElems :: (Functor m, MonadIO m) => [ElemID] -> ([Elem] -> m a) -> m a
withElems = J.withElems . map toJSStr

-- | Perform an IO action over the a list of elements matching a query
--   selector.
withElemsQS :: (IsElem e, Functor m, MonadIO m)
            => e
            -> QuerySelector
            -> ([Elem] -> m a)
            -> m a
withElemsQS el = J.withElemsQS el . toJSStr

-- | Map an IO computation over the list of elements matching a query selector.
mapQS :: (IsElem e, Functor m, MonadIO m)
      => e
      -> QuerySelector
      -> (Elem -> m a)
      -> m [a]
mapQS el = J.mapQS el . toJSStr

-- | Like @mapQS@ but returns no value.
mapQS_ :: (IsElem e, Functor m, MonadIO m)
       => e
       -> QuerySelector
       -> (Elem -> m a)
       -> m ()
mapQS_ el = J.mapQS_ el . toJSStr

-- | Get the name of the currently selected file from a file input element.
--   Any directory information is stripped, and only the actual file name is
--   returned, as the directory information is useless (and faked) anyway.
getFileName :: (IsElem e, Functor m, MonadIO m) => e -> m String
getFileName = fmap fromJSStr . J.getFileName

-- | Add or remove a class from an element's class list.
setClass :: (IsElem e, Functor m, MonadIO m) => e -> String -> Bool -> m ()
setClass e sel = J.setClass e (toJSStr sel)

-- | Toggle the existence of a class within an elements class list.
toggleClass :: (IsElem e, Functor m, MonadIO m) => e -> String -> m ()
toggleClass e = J.toggleClass e . toJSStr

-- | Does the given element have a particular class?
hasClass :: (IsElem e, Functor m, MonadIO m) => e -> String -> m Bool
hasClass e = J.hasClass e . toJSStr
