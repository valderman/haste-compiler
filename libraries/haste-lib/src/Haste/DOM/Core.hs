{-# LANGUAGE CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving,
             OverloadedStrings #-}
-- | Core types and operations for DOM manipulation.
module Haste.DOM.Core where
import Haste.Prim
import Control.Monad.IO.Class
import Haste.Foreign
import System.IO.Unsafe (unsafePerformIO)

#ifdef __HASTE__
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
#endif

-- | A DOM node.
newtype Elem = Elem JSAny
  deriving (Pack, Unpack)

-- | The class of types backed by DOM elements.
class IsElem a where
  -- | Get the element representing the object.
  elemOf :: a -> Elem

instance IsElem Elem where
  elemOf = id

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
