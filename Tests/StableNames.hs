module Tests.StableNames where
import System.Mem.StableName

foo :: Int
foo = 10

bar :: Int
bar = 11

runTest :: IO [Bool]
runTest = do
  foo' <- makeStableName foo
  bar' <- makeStableName bar
  return [foo' == foo',
          bar' == bar',
          foo' == bar']
