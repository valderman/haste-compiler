module Main where
import Haste
import Haste.DOM
import Haste.Events

main = withElems ["a","b","op","result"] calculator

calculator [a,b,op,result] = do
    onEvent a  KeyUp $ \_ -> recalculate
    onEvent b  KeyUp $ \_ -> recalculate
    onEvent op Change $ \_ -> recalculate
  where
    recalculate = do
      ma <- getValue a
      mb <- getValue b
      Just op' <- getValue op
      case (ma, mb) of
        (Just a', Just b') -> setProp result "innerHTML" (toString $ calc op' a' b')
        _                  -> return ()

    calc "+" = (+)
    calc "-" = (-)
    calc "*" = (*)
    calc "/" = (/)
    calc _   = \_ _ -> 0 :: Double
