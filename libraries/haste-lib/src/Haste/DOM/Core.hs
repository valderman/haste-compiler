{-# LANGUAGE CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving,
             OverloadedStrings #-}
-- | Core types and operations for DOM manipulation.
module Haste.DOM.Core (
    Elem (..), IsElem (..), Attribute, AttrName (..),
    set, with, attribute, children,
    click, focus, blur,
    document, documentBody,
    removeChild, clearChildren,
    setChildren, getChildren,
    getLastChild, getFirstChild, getChildBefore,
    addChildBefore, addChild    
  ) where
import Haste.Prim
import Control.Monad.IO.Class
import Haste.Foreign
import System.IO.Unsafe (unsafePerformIO)
import Data.String

#ifdef __HASTE__
foreign import ccall jsSet :: Elem -> JSString -> JSString -> IO ()
foreign import ccall jsSetAttr :: Elem -> JSString -> JSString -> IO ()
foreign import ccall jsSetStyle :: Elem -> JSString -> JSString -> IO ()
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
jsAppendChild = error "Tried to use jsAppendChild on server side!"
jsGetFirstChild = error "Tried to use jsGetFirstChild on server side!"
jsGetLastChild = error "Tried to use jsGetLastChild on server side!"
jsGetChildren = error "Tried to use jsGetChildren on server side!"
jsSetChildren = error "Tried to use jsSetChildren on server side!"
jsAddChildBefore = error "Tried to use jsAddChildBefore on server side!"
jsGetChildBefore = error "Tried to use jsGetChildBefore on server side!"
jsKillChild = error "Tried to use jsKillChild on server side!"
jsClearChildren = error "Tried to use jsClearChildren on server side!"
jsSet = error "Tried to use jsSet on server side!"
jsSetAttr = error "Tried to use jsSetAttr on server side!"
jsSetStyle = error "Tried to use jsSetStyle on server side!"
#endif

-- | A DOM node.
newtype Elem = Elem JSAny
  deriving (Pack, Unpack)

-- | The class of types backed by DOM elements.
class IsElem a where
  -- | Get the element representing the object.
  elemOf :: a -> Elem

  -- | Attempt to create an object from an 'Elem'.
  fromElem :: Elem -> IO (Maybe a)
  fromElem = const $ return Nothing

instance IsElem Elem where
  elemOf = id
  fromElem = return . Just

-- | The name of an attribute. May be either a common property, an HTML
--   attribute or a style attribute.
data AttrName
  = PropName  !JSString
  | StyleName !JSString
  | AttrName  !JSString

instance IsString AttrName where
  fromString = PropName . fromString

-- | A key/value pair representing the value of an attribute.
--   May represent a property, an HTML attribute, a style attribute or a list
--   of child elements.
data Attribute
  = Attribute !AttrName !JSString
  | Children ![Elem]

-- | Construct an 'Attribute'.
attribute :: AttrName -> JSString -> Attribute
attribute = Attribute

-- | Set a number of 'Attribute's on an element.
set :: (IsElem e, MonadIO m) => e -> [Attribute] -> m ()
set e as =
    liftIO $ mapM_ set' as
  where
    e' = elemOf e
    set' (Attribute (PropName k) v)  = jsSet e' k v
    set' (Attribute (StyleName k) v) = jsSetStyle e' k v
    set' (Attribute (AttrName k) v)  = jsSetAttr e' k v
    set' (Children cs)               = mapM_ (flip jsAppendChild e') cs

-- | Attribute adding a list of child nodes to an element.
children :: [Elem] -> Attribute
children = Children

-- | Set a number of 'Attribute's on the element produced by an IO action.
--   Gives more convenient syntax when creating elements:
--
--     newElem "div" `with` [
--         style "border" := "1px solid black",
--         ...
--       ]
--
with :: (IsElem e, MonadIO m) => m e -> [Attribute] -> m e
with m attrs = do
  x <- m
  set x attrs
  return x

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

-- | Remove all children from the given element.
clearChildren :: (IsElem e, MonadIO m) => e -> m ()
clearChildren = liftIO . jsClearChildren . elemOf

-- | Remove the first element from the second's children.
removeChild :: (IsElem parent, IsElem child, MonadIO m)
            => child
            -> parent
            -> m ()
removeChild child parent = liftIO $ jsKillChild (elemOf child) (elemOf parent)
