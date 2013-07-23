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

tagToEnumB :: Int -> Bool
tagToEnumB (I# x) = tagToEnum# x

tagToEnum :: Int -> D
tagToEnum (I# x) = tagToEnum# x

dataToTag :: a -> Int
dataToTag x = I# (dataToTag# x)

runTest :: IO (D, D, Bool, D)
runTest = do
    return (tagToEnum foo,
            tagToEnum bar,
            tagToEnumB foo,
            tagToEnum $ dataToTag A)
