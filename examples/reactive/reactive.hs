-- | Reactive version of the calculator example.
--   Shows a picture of a cat whenever the result is 1337.
module Main where
import Haste
import Haste.Reactive
import Control.Applicative

main = do
  elemProp "result.innerHTML" << result
  elemProp "theimage.src" << showCat <$> result
  where
    result = calc <$> valueOf "op"
                  <*> "a" `valueAt` OnKeyUp
                  <*> "b" `valueAt` OnKeyUp

    calc "+" = (+)
    calc "-" = (-)
    calc "*" = (*)
    calc "/" = (/)
    calc _   = \_ _ -> 0 :: Double

    showCat x | x == 1337 = "http://hem.ekblad.cc/kittah.php"
              | otherwise = ""
