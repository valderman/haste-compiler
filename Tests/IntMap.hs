module Tests.IntMap where
import qualified Data.IntMap as M

{-# NOINLINE m #-}
m :: M.IntMap Int
m = M.fromList [(4346, 1092), (1, 55), (99, 3), (7867, 726),
                (564, 42), (765, 6), (0, 9), (4, 88), (456, 0)]

runTest :: IO [(Int, Int)]
runTest
  = return
  . map (\(k, v) -> (v*2, k+4))
  . M.toList
  . M.map (+77)
  . M.insert 3298 9
  . M.insert 765 5
  . M.delete 0
  . M.update (Just . (+99)) 4
  . M.update (const Nothing) 456
  . M.update (Just . (\x -> x-1)) 1337
  $ m
