module Main where
import Haste

main :: IO ()
main = do
  ul <- elemsByQS document "ul#demo-list"
  case ul of
    (el:_) -> mapQS_ document "#demo-list li" (handleRemove el)
    _      -> error "Element 'ul#demo-list' not found"

handleRemove :: Elem -> Elem -> IO Bool
handleRemove ul li = do
  onEvent li OnClick $ \_ _ -> removeChild li ul
