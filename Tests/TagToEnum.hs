{-# LANGUAGE MagicHash, BangPatterns #-}
module Tests.TagToEnum where
import GHC.Prim
import GHC.Types

data D = A | B | C deriving Show

{-# NOINLINE foo #-}
foo :: Int
foo = 1

{-# NOINLINE bar #-}
bar :: Int
bar = 2

runTest :: IO (D, D, Bool, D)
runTest = do
    return (tagToEnum# foo',
            tagToEnum# bar',
            tagToEnum# foo',
            tagToEnum# (dataToTag# A))
  where
    {-# NOINLINE foo' #-}
    !(I# foo') = foo
    {-# NOINLINE bar' #-}
    !(I# bar') = bar