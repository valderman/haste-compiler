{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | DOM events and utilities for the Haste reactive library.
module Haste.Reactive.DOM (clicked,valueOf,valueAt,ElemProp,elemProp) where
import FRP.Fursuit
import Haste
import Haste.DOM

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

-- | An event that gets raised whenever the element with the specified ID is
--   clicked.
clicked :: ElemID -> IO (Signal ())
clicked eid = withElem eid $ \e -> do
  (p,s) <- pipe ()
  _ <- setCallback e OnClick (write p ())
  return s

-- | The value property of the given element, updated whenever an onchange
--   event is raised.
valueOf :: Readable a => ElemID -> IO (Signal a)
valueOf e = e `valueAt` OnChange

-- | The value property of the given element, triggered on a custom event.
valueAt :: Readable a => ElemID -> Event -> IO (Signal a)
valueAt eid evt = withElem eid $ \e -> do
  str <- getProp e "value"
  (src, sig) <- case fromStr str of
                  Just x -> pipe x
                  _      -> error $ "Bad initial value in valueAt: " ++ str
  
  success <- setCallback e evt $ do
    str' <- getProp e "value"
    case fromStr str' of
      Just x' -> write src x'
      _       -> return ()

  if (not success) 
     then error $ "Browser doesn't support sane event handlers!"
     else return sig

instance Showable a => Sink ElemProp a where
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
