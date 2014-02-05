{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses, 
             FlexibleContexts, OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | DOM events and utilities for the Haste reactive library.
module Haste.Reactive.DOM (clicked,valueOf,valueAt,ElemProp,elemProp) where
import FRP.Fursuit
import Haste
import qualified Data.Map as M
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

{-# NOINLINE eventHandlers #-}
-- | Contains a list of all installed event handlers.
eventHandlers :: JSType a => IORef (M.Map (ElemID, Event IO e) (Signal a))
eventHandlers = unsafePerformIO $ newIORef M.empty

-- | Represents a property of a DOM object.
data ElemProp where
  D :: ElemID -> PropID -> ElemProp

-- | Create a 'DOMObject' from a string describing the object. For example,
--   domObj "myobject.value" corresponds to the value attribute of the object
--   with the ID "myobject".
elemProp :: String -> ElemProp
elemProp str =
  case span (/= '.') str of
    ([], _)     -> error "elemProp: No object ID given!"
    (_, [])     -> error "elemProp: No object attribute given!"
    (obj, attr) -> D obj (tail attr)

unlessExists :: JSType a => ElemID -> Event IO e -> IO (Signal a) -> Signal a
unlessExists eid evt create = new $ do
  handlers <- readIORef eventHandlers
  case M.lookup (eid, evt) handlers of
    Just s -> return s
    _      -> do
      sig <- create
      writeIORef eventHandlers (M.insert (eid, evt) sig handlers)
      return sig


-- | An event that gets raised whenever the element with the specified ID is
--   clicked.
clicked :: ElemID -> Signal ()
clicked eid = unlessExists eid OnClick clickedIO
  where
    clickedIO = withElem eid $ \e -> do
      (p,s) <- pipe ()
      _ <- setCallback e OnClick (\_ _ -> write p ())
      return s

-- | The value property of the given element, updated whenever an onchange
--   event is raised.
valueOf :: JSType a  => ElemID -> Signal a
valueOf e = e `valueAt` OnChange

-- | The value property of the given element, triggered on a custom event.
valueAt :: (JSType a, Callback e) => ElemID -> Event IO e -> Signal a
valueAt eid evt = filterMapS fromString $ unlessExists eid evt valueAtIO
  where
    valueAtIO = withElem eid $ \e -> do
      str <- getProp e "value"
      (src, sig) <- pipe str
      success <- setCallback e evt $ constCallback $ do
        getProp e "value" >>= write src

      if (not success) 
        then error $ "Browser doesn't support sane event handlers!"
        else return sig

-- | Like show, but strips enclosing quotes.
toStr :: Show a => a -> String
toStr x =
  case show x of
    ('"':xs) -> init xs
    xs       -> xs

instance Show a => Sink ElemProp a where
  (D obj attr) << val = withElem obj $ \e -> sink (setProp e attr . toStr) val

-- | Replace the sink element's list of child nodes whenever a new list of
--   nodes comes down the pipe.
instance Sink Elem [Elem] where
  e << val = sink (setChildren e) val

-- | Same as the instance for [Elem].
instance Sink Elem [IO Elem] where
  e << val = sink (\children -> sequence children >>= setChildren e) val

-- | Set the sink element's innerHTML property whenever a new string comes down
--   the pipe.
instance Sink Elem String where
  e << val = sink (setProp e "innerHTML") val
