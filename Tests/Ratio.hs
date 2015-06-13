module Tests.Ratio where
import Data.Ratio

runTest = do
  print $ (1 % 4)                 -- Pass
  print $ ((1 % 4) :: Ratio Int)  -- Fail
  print $ ((1 % 4) :: TypeT)      -- Fail

  print $ DataT {d1 = 1 % 4}      -- Fail
  print $ DataT (1 % 4)           -- Fail
  print $ d3 $ NewtypeT (1 % 4)   -- Fail


data DataT = DataT {d1 :: Ratio Int} deriving (Show)

newtype NewtypeT = NewtypeT { d3 :: Ratio Int }

type TypeT = Ratio Int
