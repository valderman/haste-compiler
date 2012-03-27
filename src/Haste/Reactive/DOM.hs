-- | DOM events and utilities for the Haste reactive library.
module Haste.Reactive.DOM (valueOf, valueAt, bind, (=:)) where
import Haste
import Haste.Reactive.Signal

-- | The value property of the given element, updated whenever an onchange
--   event is raised.
valueOf :: Readable a => ElemID -> IO (Signal a)
valueOf e = e `valueAt` OnChange

-- | The value property of the given element, triggered on a custom event.
valueAt :: Readable a => ElemID -> Event -> IO (Signal a)
valueAt e evt = do
  str <- getProp e "value"
  (src, sig) <- case fromStr str of
                  Just x -> source x
                  _      -> error $ "Bad initial value in valueAt: " ++ str
  
  success <- setCallback e evt $ do
    str' <- getProp e "value"
    case fromStr str' of
      Just x' -> push x' src
      _       -> return ()

  if (not success) 
     then error $ "Not found: " ++ e
     else return sig

-- | When the given signal fires, write its result to the given property of
--   the given element.
bind :: Showable a => ElemID -> PropID -> Signal a -> IO ()
bind el pr = sink $ \x -> setProp el pr (toStr x)

-- | Infix version of 'bind'.
(=:) :: Showable a => (ElemID, PropID) -> Signal a -> IO ()
(el, pr) =: x = bind el pr x
infixl 0 =:
