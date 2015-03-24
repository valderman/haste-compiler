-- | Tests mutually recursive closures inside a tail recursive function
module Tests.TailrecClosures where
import Data.Set (Set)
import qualified Data.Set as S

runTest :: IO [String]
runTest = return $ S.toList $ S.map S.toList $ cartProd xs ys

xs = S.fromList "abc"
ys = S.fromList "def"

cartProd xs ys = S.fold outerFold S.empty xs
  where outerFold x zss = S.fold (innerFold x) S.empty ys `S.union` zss
        innerFold x y zs = S.fromList [x,y] `S.insert` zs
