{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
-- | DOM manipulation functions using 'JSString' for string representation.
module Haste.DOM.JSString (
    -- From Haste.DOM.Core
    AttrName (..), Attribute, IsElem (..), Elem (..),
    attribute, set, with, children,
    click, focus, blur, document, documentBody, appendChild, addChild,
    addChildBefore, insertChildBefore, getFirstChild, getLastChild, getChildren,
    setChildren, clearChildren, deleteChild, removeChild,
    -- Own exports
    PropID, ElemID, QuerySelector, ElemClass, AttrValue,
    prop, style, attr, (=:),
    newElem, newTextElem,
    elemById, elemsByQS, elemsByClass,
    setProp, getProp, setAttr, getAttr, getValue,
    withElem , withElems, withElemsQS, mapQS, mapQS_,
    getStyle, setStyle,
    getFileData, getFileName,
    setClass, toggleClass, hasClass
  ) where
import Haste.Prim
import Haste.Prim.JSType
import Haste.DOM.Core
import Data.Maybe (isNothing, fromJust)
import Control.Monad.IO.Class
import Haste.Foreign
import Haste.Binary.Types

type PropID = JSString
type ElemID = JSString
type QuerySelector = JSString
type ElemClass = JSString
type AttrValue = JSString

jsGet :: Elem -> JSString -> IO JSString
jsGet = ffi "(function(e,p){var x = e[p];\
  \return typeof x === 'undefined' ? '' : x.toString();})"

jsGetAttr :: Elem -> JSString -> IO JSString
jsGetAttr = ffi "(function(e,p){\
\return e.hasAttribute(p) ? e.getAttribute(p) : '';})"

jsGetStyle :: Elem -> JSString -> IO JSString
jsGetStyle = ffi "(function(e,p){var x = e.style[p];\
  \return typeof x === 'undefined' ? '' : x.toString();})"

jsFind :: JSString -> IO (Maybe Elem)
jsFind = ffi "(function(id){return document.getElementById(id);})"

jsQuerySelectorAll :: Elem -> JSString -> IO [Elem]
jsQuerySelectorAll = ffi "(function(e,q){\
  \if(!e || typeof e.querySelectorAll !== 'function') {\
    \return [];\
  \} else {\
    \return e.querySelectorAll(q);\
  \}})"

jsElemsByClassName :: JSString -> IO [Elem]
jsElemsByClassName = ffi "(function(c){\
\return document.getElementsByClassName(c);})"

jsCreateElem :: JSString -> IO Elem
jsCreateElem = ffi "(function(t){return document.createElement(t);})"

jsCreateTextNode :: JSString -> IO Elem
jsCreateTextNode = ffi "(function(s){return document.createTextNode(s);})"

-- | Create a style attribute name.
style :: JSString -> AttrName
style = StyleName

-- | Create an HTML attribute name.
attr :: JSString -> AttrName
attr = AttrName

-- | Create a DOM property name.
--   See <http://stackoverflow.com/questions/6003819/properties-and-attributes-in-html> for more information about the difference between attributes and properties.
prop :: JSString -> AttrName
prop = PropName

-- | Create an 'Attribute'.
infixl 4 =:
(=:) :: AttrName -> AttrValue -> Attribute
(=:) = attribute

-- | Create an element.
newElem :: MonadIO m => JSString -> m Elem
newElem = liftIO . jsCreateElem

-- | Create a text node.
newTextElem :: MonadIO m => JSString -> m Elem
newTextElem = liftIO . jsCreateTextNode

-- | Set a property of the given element.
setProp :: (IsElem e, MonadIO m) => e -> PropID -> JSString -> m ()
setProp e property val = liftIO $ jsSet (elemOf e) property val

-- | Set an attribute of the given element.
setAttr :: (IsElem e, MonadIO m) => e -> PropID -> JSString -> m ()
setAttr e property val = liftIO $ jsSetAttr (elemOf e) property val

-- | Get the value property of an element; a handy shortcut.
getValue :: (IsElem e, MonadIO m, JSType a) => e -> m (Maybe a)
getValue e = liftIO $ fromJSString `fmap` jsGet (elemOf e) "value"

-- | Get a property of an element.
getProp :: (IsElem e, MonadIO m) => e -> PropID -> m JSString
getProp e property = liftIO $ jsGet (elemOf e) property

-- | Get an attribute of an element.
getAttr :: (IsElem e, MonadIO m) => e -> PropID -> m JSString
getAttr e property = liftIO $ jsGetAttr (elemOf e) property

-- | Get a CSS style property of an element.
getStyle :: (IsElem e, MonadIO m) => e -> PropID -> m JSString
getStyle e property = liftIO $ jsGetStyle (elemOf e) property

-- | Set a CSS style property on an element.
setStyle :: (IsElem e, MonadIO m) => e -> PropID -> JSString -> m ()
setStyle e property val = liftIO $ jsSetStyle (elemOf e) property val

-- | Get an element by its HTML ID attribute.
elemById :: MonadIO m => ElemID -> m (Maybe Elem)
elemById eid = liftIO $ jsFind eid

-- | Get all elements of the given class.
elemsByClass :: MonadIO m => ElemClass -> m [Elem]
elemsByClass cls = liftIO $ jsElemsByClassName cls

-- | Get all children elements matching a query selector.
elemsByQS :: (IsElem e, MonadIO m) => e -> QuerySelector -> m [Elem]
elemsByQS el sel = liftIO $ jsQuerySelectorAll (elemOf el) sel

-- | Perform an IO action on an element.
withElem :: MonadIO m => ElemID -> (Elem -> m a) -> m a
withElem e act = do
  me' <- elemById e
  case me' of
    Just e' -> act e'
    _       -> error $ "No element with ID " ++ fromJSStr e ++ " found!"

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

-- | Perform an IO action over the a list of elements matching a query
--   selector.
withElemsQS :: (IsElem e, MonadIO m)
            => e
            -> QuerySelector
            -> ([Elem] -> m a)
            -> m a
withElemsQS el sel act = elemsByQS el sel >>= act

-- | Map an IO computation over the list of elements matching a query selector.
mapQS :: (IsElem e, MonadIO m) => e -> QuerySelector -> (Elem -> m a) -> m [a]
mapQS el sel act = elemsByQS el sel >>= mapM act

-- | Like @mapQS@ but returns no value.
mapQS_ :: (IsElem e, MonadIO m) => e -> QuerySelector -> (Elem -> m a) -> m ()
mapQS_ el sel act = elemsByQS el sel >>= mapM_ act

-- | Get a file from a file input element.
getFileData :: (IsElem e, MonadIO m) => e -> Int -> m (Maybe Blob)
getFileData e ix = liftIO $ do
    num <- getFiles (elemOf e)
    if ix < num
      then Just `fmap` getFile (elemOf e) ix
      else return Nothing

getFiles :: Elem -> IO Int
getFiles = ffi "(function(e){return e.files.length;})"

getFile :: Elem -> Int -> IO Blob
getFile = ffi "(function(e,ix){return e.files[ix];})"

-- | Get the name of the currently selected file from a file input element.
--   Any directory information is stripped, and only the actual file name is
--   returned, as the directory information is useless (and faked) anyway.
getFileName :: (IsElem e, MonadIO m) => e -> m JSString
getFileName e = liftIO $ do
    fn <- fromJSStr `fmap` getProp e "value"
    return $ toJSStr $ reverse $ takeWhile (not . separator) $ reverse fn
  where
    separator '/'  = True
    separator '\\' = True
    separator _    = False

-- | Add or remove a class from an element's class list.
setClass :: (IsElem e, MonadIO m) => e -> JSString -> Bool -> m ()
setClass e c x = liftIO $ setc (elemOf e) c x

setc :: Elem -> JSString -> Bool -> IO ()
setc = ffi "(function(e,c,x){x?e.classList.add(c):e.classList.remove(c);})"

-- | Toggle the existence of a class within an elements class list.
toggleClass :: (IsElem e, MonadIO m) => e -> JSString -> m ()
toggleClass e c = liftIO $ toggc (elemOf e) c

toggc :: Elem -> JSString -> IO ()
toggc = ffi "(function(e,c) {e.classList.toggle(c);})"

-- | Does the given element have a particular class?
hasClass :: (IsElem e, MonadIO m) => e -> JSString -> m Bool
hasClass e c = liftIO $ getc (elemOf e) c

getc :: Elem -> JSString -> IO Bool
getc = ffi "(function(e,c) {return e.classList.contains(c);})"
