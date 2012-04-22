{-# LANGUAGE DeriveDataTypeable #-}
module Tests.Exception where
import Prelude hiding (catch)
import Data.Typeable
import Control.Exception

newtype TestException = TE String deriving (Show, Typeable)
instance Exception TestException

printTE :: TestException -> IO String
printTE (TE s) = return s

runTest :: IO String
runTest = do
  a <- catch (throw $ TE "So I heard you like ") printTE
  b <- catch (return "mudkips...") printTE
  return (a ++ b)
