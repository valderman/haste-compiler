{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses #-}
-- | DOM events and utilities for the Haste reactive library.
module Haste.Reactive.DOM (clicked, valueOf, valueAt, DOMObject, domObj) where
import FRP.Fursuit
import Haste
import Data.String

-- | Represents a DOM object; a DOM object consists of an object ID,
--   corresponding to the object's ID attribute in the HTML, and an attribute.
data DOMObject a where
  D :: (Showable a, Readable a) => ElemID -> PropID -> DOMObject a

-- | Create a 'DOMObject' from a string describing the object. For example,
--   domObj "myobject.value" corresponds to the value attribute of the object
--   with the ID "myobject".
domObj :: (Showable a, Readable a) => String -> DOMObject a
domObj str =
  case span (/= '.') str of
    ([], _)     -> error "domObj: No object ID given!"
    (_, [])     -> error "domObj: No object attribute given!"
    (obj, attr) -> D obj (tail attr)

instance (Showable a, Readable a) => IsString (DOMObject a) where
  fromString = domObj

-- | An event that gets raised whenever the element with the specified ID is
--   clicked.
clicked :: ElemID -> IO (Signal ())
clicked e = do
  (p,s) <- pipe ()
  _ <- setCallback e OnClick (write p ())
  return s

-- | The value property of the given element, updated whenever an onchange
--   event is raised.
valueOf :: Readable a => ElemID -> IO (Signal a)
valueOf e = e `valueAt` OnChange

-- | The value property of the given element, triggered on a custom event.
valueAt :: Readable a => ElemID -> Event -> IO (Signal a)
valueAt e evt = do
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
     then error $ "Not found: " ++ e
     else return sig

instance Sink (DOMObject a) a where
  (D obj attr) << val = sink (setProp obj attr . toStr) val
