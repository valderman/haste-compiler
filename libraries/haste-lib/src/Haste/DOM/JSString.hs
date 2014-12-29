{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, CPP,
             GeneralizedNewtypeDeriving #-}
-- | DOM manipulation functions using 'JSString' for string representation.
module Haste.DOM.JSString (
    IsElem (..), Elem (..), PropID, ElemID, QuerySelector, ElemClass,
    newElem, newTextElem,
    elemById, elemsByQS, elemsByClass,
    setProp, getProp, setAttr, getAttr, getValue,
    withElem , withElems, withElemsQS, mapQS, mapQS_,
    addChild, addChildBefore, removeChild, clearChildren , getChildBefore,
    getFirstChild, getLastChild, getChildren, setChildren,
    getStyle, setStyle,
    getFileData, getFileName,
    setClass, toggleClass, hasClass,
    click, focus, blur,
    document, documentBody
  ) where
import Haste.Prim
import Haste.JSType
import Data.Maybe (isNothing, fromJust)
import Control.Monad.IO.Class
import Haste.Foreign
import Haste.Binary.Types
import System.IO.Unsafe (unsafePerformIO)

newtype Elem = Elem JSAny
  deriving (Pack, Unpack)

type PropID = JSString
type ElemID = JSString
type QuerySelector = JSString
type ElemClass = JSString

#ifdef __HASTE__
foreign import ccall jsGet :: Elem -> JSString -> IO JSString
foreign import ccall jsSet :: Elem -> JSString -> JSString -> IO ()
foreign import ccall jsGetAttr :: Elem -> JSString -> IO JSString
foreign import ccall jsSetAttr :: Elem -> JSString -> JSString -> IO ()
foreign import ccall jsGetStyle :: Elem -> JSString -> IO JSString
foreign import ccall jsSetStyle :: Elem -> JSString -> JSString -> IO ()
foreign import ccall jsFind :: JSString -> IO (Ptr (Maybe Elem))
foreign import ccall jsQuerySelectorAll :: Elem -> JSString -> IO (Ptr [Elem])
foreign import ccall jsElemsByClassName :: JSString -> IO (Ptr [Elem])
foreign import ccall jsCreateElem :: JSString -> IO Elem
foreign import ccall jsCreateTextNode :: JSString -> IO Elem
foreign import ccall jsAppendChild :: Elem -> Elem -> IO ()
foreign import ccall jsGetFirstChild :: Elem -> IO (Ptr (Maybe Elem))
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
jsElemsByClassName = error "Tried to use jsElemsByClassName on server side!"
jsQuerySelectorAll = error "Tried to use jsQuerySelectorAll on server side!"
jsCreateElem = error "Tried to use jsCreateElem on server side!"
jsCreateTextNode = error "Tried to use jsCreateTextNode on server side!"
jsAppendChild = error "Tried to use jsAppendChild on server side!"
jsGetFirstChild = error "Tried to use jsGetFirstChild on server side!"
jsGetLastChild = error "Tried to use jsGetLastChild on server side!"
jsGetChildren = error "Tried to use jsGetChildren on server side!"
jsSetChildren = error "Tried to use jsSetChildren on server side!"
jsAddChildBefore = error "Tried to use jsAddChildBefore on server side!"
jsGetChildBefore = error "Tried to use jsGetChildBefore on server side!"
jsKillChild = error "Tried to use jsKillChild on server side!"
jsClearChildren = error "Tried to use jsClearChildren on server side!"
#endif

-- | The class of types backed by DOM elements.
class IsElem a where
  -- | Get the element representing the object.
  elemOf :: a -> Elem

instance IsElem Elem where
  elemOf = id

-- | Append the first element as a child of the second element.
addChild :: (IsElem parent, IsElem child, MonadIO m) => child -> parent -> m ()
addChild child parent = liftIO $ jsAppendChild (elemOf child) (elemOf parent)

-- | Insert the first element as a child into the second, before the third.
--   For instance:
-- @
--   addChildBefore childToAdd theContainer olderChild
-- @
addChildBefore :: (IsElem parent, IsElem child, MonadIO m)
               => child -> parent -> child -> m ()
addChildBefore child parent oldChild =
  liftIO $ jsAddChildBefore (elemOf child) (elemOf parent) (elemOf oldChild)

-- | Get the sibling before the given one, if any.
getChildBefore :: (IsElem e, MonadIO m) => e -> m (Maybe Elem)
getChildBefore e = liftIO $ fromPtr `fmap` jsGetChildBefore (elemOf e)

-- | Get the first of an element's children.
getFirstChild :: (IsElem e, MonadIO m) => e -> m (Maybe Elem)
getFirstChild e = liftIO $ fromPtr `fmap` jsGetFirstChild (elemOf e)

-- | Get the last of an element's children.
getLastChild :: (IsElem e, MonadIO m) => e -> m (Maybe Elem)
getLastChild e = liftIO $ fromPtr `fmap` jsGetLastChild (elemOf e)

-- | Get a list of all children belonging to a certain element.
getChildren :: (IsElem e, MonadIO m) => e -> m [Elem]
getChildren e = liftIO $ fromPtr `fmap` jsGetChildren (elemOf e)

-- | Clear the given element's list of children, and append all given children
--   to it.
setChildren :: (IsElem parent, IsElem child, MonadIO m)
            => parent
            -> [child]
            -> m ()
setChildren e ch = liftIO $ jsSetChildren (elemOf e) (toPtr $ map elemOf ch)

-- | Create an element.
newElem :: MonadIO m => JSString -> m Elem
newElem = liftIO . jsCreateElem

-- | Create a text node.
newTextElem :: MonadIO m => JSString -> m Elem
newTextElem = liftIO . jsCreateTextNode

-- | Set a property of the given element.
setProp :: (IsElem e, MonadIO m) => e -> PropID -> JSString -> m ()
setProp e prop val = liftIO $ jsSet (elemOf e) prop val

-- | Set an attribute of the given element.
setAttr :: (IsElem e, MonadIO m) => e -> PropID -> JSString -> m ()
setAttr e prop val = liftIO $ jsSetAttr (elemOf e) prop val

-- | Get the value property of an element; a handy shortcut.
getValue :: (IsElem e, MonadIO m, JSType a) => e -> m (Maybe a)
getValue e = liftIO $ fromJSString `fmap` jsGet (elemOf e) "value"

-- | Get a property of an element.
getProp :: (IsElem e, MonadIO m) => e -> PropID -> m JSString
getProp e prop = liftIO $ jsGet (elemOf e) prop

-- | Get an attribute of an element.
getAttr :: (IsElem e, MonadIO m) => e -> PropID -> m JSString
getAttr e prop = liftIO $ jsGetAttr (elemOf e) prop

-- | Get a CSS style property of an element.
getStyle :: (IsElem e, MonadIO m) => e -> PropID -> m JSString
getStyle e prop = liftIO $ jsGetStyle (elemOf e) prop

-- | Set a CSS style property on an element.
setStyle :: (IsElem e, MonadIO m) => e -> PropID -> JSString -> m ()
setStyle e prop val = liftIO $ jsSetStyle (elemOf e) prop val

-- | Get an element by its HTML ID attribute.
elemById :: MonadIO m => ElemID -> m (Maybe Elem)
elemById eid = liftIO $ fromPtr `fmap` (jsFind eid)

-- | Get all elements of the given class.
elemsByClass :: MonadIO m => ElemClass -> m [Elem]
elemsByClass cls = liftIO $ fromPtr `fmap` (jsElemsByClassName cls)

-- | Get all children elements matching a query selector.
elemsByQS :: (IsElem e, MonadIO m) => e -> QuerySelector -> m [Elem]
elemsByQS el sel = liftIO $ fromPtr `fmap` (jsQuerySelectorAll (elemOf el) sel)

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

-- | Remove all children from the given element.
clearChildren :: (IsElem e, MonadIO m) => e -> m ()
clearChildren = liftIO . jsClearChildren . elemOf

-- | Remove the first element from the second's children.
removeChild :: (IsElem parent, IsElem child, MonadIO m)
            => child
            -> parent
            -> m ()
removeChild child parent = liftIO $ jsKillChild (elemOf child) (elemOf parent)

-- | Get a file from a file input element.
getFileData :: (IsElem e, MonadIO m) => e -> Int -> m (Maybe Blob)
getFileData e ix = liftIO $ do
    num <- getFiles (elemOf e)
    if ix < num
      then Just `fmap` getFile (elemOf e) ix
      else return Nothing
  where
    {-# NOINLINE getFiles #-}
    getFiles :: Elem -> IO Int
    getFiles = ffi "(function(e){return e.files.length;})"
    {-# NOINLINE getFile #-}
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
  where
    {-# NOINLINE setc #-}
    setc :: Elem -> JSString -> Bool -> IO ()
    setc = ffi "(function(e,c,x){x?e.classList.add(c):e.classList.remove(c);})"

-- | Toggle the existence of a class within an elements class list.
toggleClass :: (IsElem e, MonadIO m) => e -> JSString -> m ()
toggleClass e c = liftIO $ toggc (elemOf e) c
  where
    {-# NOINLINE toggc #-}
    toggc :: Elem -> JSString -> IO ()
    toggc = ffi "(function(e,c) {e.classList.toggle(c);})"

-- | Does the given element have a particular class?
hasClass :: (IsElem e, MonadIO m) => e -> JSString -> m Bool
hasClass e c = liftIO $ getc (elemOf e) c
  where
    {-# NOINLINE getc #-}
    getc :: Elem -> JSString -> IO Bool
    getc = ffi "(function(e,c) {return e.classList.contains(c);})"

-- | Generate a click event on an element.
click :: (IsElem e, MonadIO m) => e -> m ()
click = liftIO . click' . elemOf
  where
    {-# NOINLINE click' #-}
    click' :: Elem -> IO ()
    click' = ffi "(function(e) {e.click();})"

-- | Generate a focus event on an element.
focus :: (IsElem e, MonadIO m) => e -> m ()
focus = liftIO . focus' . elemOf
  where
    {-# NOINLINE focus' #-}
    focus' :: Elem -> IO ()
    focus' = ffi "(function(e) {e.focus();})"

-- | Generate a blur event on an element.
blur :: (IsElem e, MonadIO m) => e -> m ()
blur = liftIO . blur' . elemOf
  where
    {-# NOINLINE blur' #-}
    blur' :: Elem -> IO ()
    blur' = ffi "(function(e) {e.blur();})"

-- | The DOM node corresponding to document.
document :: Elem
document = unsafePerformIO getDocument
  where
    {-# NOINLINE getDocument #-}
    getDocument :: IO Elem
    getDocument = ffi "document"

-- | The DOM node corresponding to document.body.
documentBody :: Elem
documentBody = unsafePerformIO getBody
  where
    {-# NOINLINE getBody #-}
    getBody :: IO Elem
    getBody = ffi "document.body"
