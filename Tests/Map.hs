module Tests.Map where
import qualified Data.Map as M

{-# NOINLINE m #-}
m :: M.Map String Int
m = M.fromList [("asd", 1092), ("njx.,", 55), ("sad", 3), ("durr", 726),
                ("42", 42), ("five", 6), ("oas", 9), ("derp", 88), ("\"\"", 0)]

runTest :: IO [(Int, String)]
runTest
  = return
  . map (\(k, v) -> (v*2, reverse k))
  . M.toList
  . M.map (+77)
  . M.insert "cirno" 9
  . M.insert "five" 5
  . M.delete "sad"
  . M.update (Just . (+99)) "njx.,"
  . M.update (const Nothing) "oas"
  . M.update (Just . (\x -> x-1)) "not here!"
  $ m
