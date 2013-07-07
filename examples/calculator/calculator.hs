module Main where
import Haste

main = withElems ["a","b","op","result"] calculator

calculator [a,b,op,result] = do
    onEvent a  OnKeyUp $ \_ -> recalculate
    onEvent b  OnKeyUp $ \_ -> recalculate
    onEvent op OnChange $ recalculate
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
