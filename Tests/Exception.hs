{-# LANGUAGE DeriveDataTypeable #-}
module Tests.Exception where
import Prelude
import Data.Typeable
import Control.Exception as E

newtype TestException = TE String deriving (Show, Typeable)
instance Exception TestException

printTE :: TestException -> IO String
printTE (TE s) = return s

runTest :: IO String
runTest = do
  a <- E.catch (throw $ TE "So I heard you like ") printTE
  b <- E.catch (return "mudkips...") printTE
  return (a ++ b)
