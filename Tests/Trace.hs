module Tests.Trace where
import Debug.Trace

runTest = do
  x <- return $ trace "'sup" "hello"
  putStrLn $ trace "bye" x
