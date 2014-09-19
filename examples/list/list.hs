module Main where
import Haste

main :: IO ()
main = do
  ul <- elemByQuerySelector document "ul#demo-list"
  case ul of
    Just el -> withQuerySelectorElems document "#demo-list li" (handleRemoves el)
    _       -> error "Element 'ul#demo-list' not found"

handleRemoves :: Elem -> [Elem] -> IO ()
handleRemoves ul items = do
  _ <- mapM (handleRemove ul) items
  return ()

handleRemove :: Elem -> Elem -> IO Bool
handleRemove ul li = do
  onEvent li OnClick $ \_ _ -> removeChild li ul
