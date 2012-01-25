module CodeGen.Javascript.Bag (
    Bag, cons, snoc, concat, empty, singleton, fromList, toList, catLst
  ) where
import Prelude hiding (concat)
import Data.List (foldl')

data Bag a
  = Nil
  | Concat (Bag a) (Bag a)
  | Cons a (Bag a)
  | Snoc (Bag a) a

cons :: a -> Bag a -> Bag a
cons = Cons

snoc :: Bag a -> a -> Bag a
snoc = Snoc

concat :: Bag a -> Bag a -> Bag a
concat = Concat

catLst :: [Bag a] -> Bag a
catLst = foldl' concat empty

fromList :: [a] -> Bag a
fromList = foldl' snoc empty

singleton :: a -> Bag a
singleton a = a `cons` Nil

empty :: Bag a
empty = Nil

-- | Produce a list from the given bag.
toList :: Bag a -> [a]
toList bag = go bag []
  where
    go Nil xs =
      xs
    go (Concat a b) xs =
      go a (go b xs)
    go (Cons a as) xs =
      a : go as xs
    go (Snoc as a) xs =
      go as (a:xs)
