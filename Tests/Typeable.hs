module Tests.Typeable where
import Data.Typeable

runTest :: IO [String]
runTest = do
  let a = if typeOf (Just 6 :: Maybe Int) == typeOf (Nothing :: Maybe Int)
            then ":)"
            else ":("
      b = if typeOf "hi!" == typeOf "samurai santa"
            then ":)"
            else ":("
  return [a, b]
