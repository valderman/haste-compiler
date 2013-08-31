module Tests.PutStr where

runTest :: IO ()
runTest = do
  mapM_ putStr $ words "this is a list of words without spaces"
  mapM_ putStrLn $ words "and this is a list of words WITH spaces"
  putStrLn $ unlines $ words "finally, words with newlines"
