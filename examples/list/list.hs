module Main where
import Haste
import Haste.DOM
import Haste.Events

main :: IO ()
main = do
  ul <- elemsByQS document "ul#demo-list"
  case ul of
    (el:_) -> mapQS_ document "#demo-list li" (handleRemove el)
    _      -> error "Element 'ul#demo-list' not found"

handleRemove :: Elem -> Elem -> IO HandlerInfo
handleRemove ul li = do
  onEvent li Click $ \_ -> do
    removeChild li ul
    preventDefault
