{-# OPTIONS -fno-warn-overlapping-patterns #-}
module Test.TestUtil
    (
    module Test.TestUtil,
    module Test.Framework,
    module Test.Framework.Providers.QuickCheck2
    ) where

import Test.Framework
import Test.Framework.Providers.API
import Test.Framework.Providers.QuickCheck2
import Data.Typeable

data Result = Pass | Fail String deriving Typeable

instance Show Result where
    show Pass = "passed"
    show (Fail s) = "failed: " ++ s

instance TestResultlike () Result where
    testSucceeded Pass = True
    testSucceeded (Fail _) = False

instance Testlike () Result (IO Result) where
    testTypeName _ = "Cases"
    runTest _ ior = do
        r <- ior
        return (Finished r,return ())

ioTest :: String -> IO Result -> Test
ioTest = Test

pureTest :: String -> Result -> Test
pureTest name result = ioTest name (return result)

diff :: (Show a,Eq a) => a -> a -> Result
diff expected found | expected == found = Pass
diff expected found = Fail ("expected " ++ (show expected) ++ " but found " ++ (show found))

