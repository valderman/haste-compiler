{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, OverloadedStrings #-}
-- | Core types and operations for DOM manipulation.
module Haste.DOM.Core (
    Elem (..), IsElem (..), Attribute, AttrName (..),
    set, with, attribute, children,
    click, focus, blur,
    document, documentBody,
    deleteChild, clearChildren,
    setChildren, getChildren,
    getLastChild, getFirstChild, getChildBefore,
    insertChildBefore, appendChild,
    -- Low level stuff
    jsSet, jsSetAttr, jsSetStyle,
    -- Deprecated
    removeChild, addChild, addChildBefore
  ) where
import Haste.Prim
import Control.Monad.IO.Class
import Haste.Prim.Foreign
import Data.String

jsSet :: Elem -> JSString -> JSString -> IO ()
jsSet = ffi "(function(e,p,v){e[p] = v;})"

jsSetAttr :: Elem -> JSString -> JSString -> IO ()
jsSetAttr = ffi "(function(e,p,v){e.setAttribute(p, v);})"

jsSetStyle :: Elem -> JSString -> JSString -> IO ()
jsSetStyle = ffi "(function(e,p,v){e.style[p] = v;})"

jsAppendChild :: Elem -> Elem -> IO ()
jsAppendChild = ffi "(function(c,p){p.appendChild(c);})"

jsGetFirstChild :: Elem -> IO (Maybe Elem)
jsGetFirstChild = ffi "(function(e){\
for(e = e.firstChild; e != null; e = e.nextSibling)\
  {if(e instanceof HTMLElement) {return e;}}\
return null;})"

jsGetLastChild :: Elem -> IO (Maybe Elem)
jsGetLastChild = ffi "(function(e){\
for(e = e.lastChild; e != null; e = e.previousSibling)\
  {if(e instanceof HTMLElement) {return e;}}\
return null;})"

jsGetChildren :: Elem -> IO [Elem]
jsGetChildren = ffi "(function(e){\
var ch = [];\
for(e = e.firstChild; e != null; e = e.nextSibling)\
  {if(e instanceof HTMLElement) {ch.push(e);}}\
return ch;})"

jsSetChildren :: Elem -> [Elem] -> IO ()
jsSetChildren = ffi "(function(e,ch){\
while(e.firstChild) {e.removeChild(e.firstChild);}\
for(var i in ch) {e.appendChild(ch[i]);}})"

jsAddChildBefore :: Elem -> Elem -> Elem -> IO ()
jsAddChildBefore = ffi "(function(c,p,a){p.insertBefore(c,a);})"

jsGetChildBefore :: Elem -> IO (Maybe Elem)
jsGetChildBefore = ffi "(function(e){\
for(; e != null; e = e.previousSibling)\
  {if(e instanceof HTMLElement) {return e;}\
return null;})"

jsKillChild :: Elem -> Elem -> IO ()
jsKillChild = ffi "(function(c,p){p.removeChild(c);})"

jsClearChildren :: Elem -> IO ()
jsClearChildren = ffi "(function(e){\
while(e.firstChild){e.removeChild(e.firstChild);}})"

-- | A DOM node.
newtype Elem = Elem JSAny
  deriving (ToAny, FromAny)

-- | The class of types backed by DOM elements.
class IsElem a where
  -- | Get the element representing the object.
  elemOf :: a -> Elem

  -- | Attempt to create a DOM element backed object from an 'Elem'.
  --   The default instance always returns @Nothing@.
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
  deriving (Eq, Ord)

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
--   > newElem "div" `with` [
--   >     style "border" =: "1px solid black",
--   >     ...
--   >   ]
--
with :: (IsElem e, MonadIO m) => m e -> [Attribute] -> m e
with m attrs = do
  x <- m
  set x attrs
  return x

-- | Generate a click event on an element.
click :: (IsElem e, MonadIO m) => e -> m ()
click = liftIO . click' . elemOf

click' :: Elem -> IO ()
click' = ffi "(function(e) {e.click();})"

-- | Generate a focus event on an element.
focus :: (IsElem e, MonadIO m) => e -> m ()
focus = liftIO . focus' . elemOf

focus' :: Elem -> IO ()
focus' = ffi "(function(e) {e.focus();})"

-- | Generate a blur event on an element.
blur :: (IsElem e, MonadIO m) => e -> m ()
blur = liftIO . blur' . elemOf

blur' :: Elem -> IO ()
blur' = ffi "(function(e) {e.blur();})"

-- | The DOM node corresponding to document.
document :: Elem
document = constant "document"

-- | The DOM node corresponding to document.body.
documentBody :: Elem
documentBody = constant "document.body"

-- | Append the second element as a child of the first.
appendChild :: (IsElem parent, IsElem child, MonadIO m) => parent -> child -> m ()
appendChild parent child = liftIO $ jsAppendChild (elemOf child) (elemOf parent)

{-# DEPRECATED addChild "Use appendChild instead. Note that appendChild == flip addChild." #-}
-- | Append the first element as a child of the second element.
addChild :: (IsElem parent, IsElem child, MonadIO m) => child -> parent -> m ()
addChild = flip appendChild

-- | Insert an element into a container, before another element.
--   For instance:
-- @
--   insertChildBefore theContainer olderChild childToAdd
-- @
insertChildBefore :: (IsElem parent, IsElem before, IsElem child, MonadIO m)
               => parent -> before -> child -> m ()
insertChildBefore parent oldChild child =
  liftIO $ jsAddChildBefore (elemOf child) (elemOf parent) (elemOf oldChild)

{-# DEPRECATED addChildBefore "Use insertChildBefore instead. Note insertChildBefore == \\parent new old -> addChildBefore new parent old." #-}
-- | Insert an element into a container, before another element.
--   For instance:
-- @
--   addChildBefore childToAdd theContainer olderChild
-- @
addChildBefore :: (IsElem parent, IsElem child, MonadIO m)
               => child -> parent -> child -> m ()
addChildBefore child parent oldChild = insertChildBefore parent oldChild child

-- | Get the sibling before the given one, if any.
getChildBefore :: (IsElem e, MonadIO m) => e -> m (Maybe Elem)
getChildBefore e = liftIO $ jsGetChildBefore (elemOf e)

-- | Get the first of an element's children.
getFirstChild :: (IsElem e, MonadIO m) => e -> m (Maybe Elem)
getFirstChild e = liftIO $ jsGetFirstChild (elemOf e)

-- | Get the last of an element's children.
getLastChild :: (IsElem e, MonadIO m) => e -> m (Maybe Elem)
getLastChild e = liftIO $ jsGetLastChild (elemOf e)

-- | Get a list of all children belonging to a certain element.
getChildren :: (IsElem e, MonadIO m) => e -> m [Elem]
getChildren e = liftIO $ jsGetChildren (elemOf e)

-- | Clear the given element's list of children, and append all given children
--   to it.
setChildren :: (IsElem parent, IsElem child, MonadIO m)
            => parent
            -> [child]
            -> m ()
setChildren e ch = liftIO $ jsSetChildren (elemOf e) (map elemOf ch)

-- | Remove all children from the given element.
clearChildren :: (IsElem e, MonadIO m) => e -> m ()
clearChildren = liftIO . jsClearChildren . elemOf

-- | Remove the second element from the first's children.
deleteChild :: (IsElem parent, IsElem child, MonadIO m)
            => parent
            -> child
            -> m ()
deleteChild parent child = liftIO $ jsKillChild (elemOf child) (elemOf parent)

{-# DEPRECATED removeChild "Use deleteChild instead. Note that deleteChild = flip removeChild." #-}
-- | DEPRECATED: use 'deleteChild' instead!
--   Note that @deleteChild = flip removeChild@.
removeChild :: (IsElem parent, IsElem child, MonadIO m)
            => child
            -> parent
            -> m ()
removeChild = flip deleteChild
