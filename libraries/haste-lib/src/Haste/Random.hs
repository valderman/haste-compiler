{-# LANGUAGE ForeignFunctionInterface #-}
module Haste.Random (Random (..), Seed, next, mkSeed, newSeed) where
import Haste.Prim
import Data.Int
import Data.Word
import Data.List (unfoldr)

foreign import ccall jsRand :: IO Double

newtype Seed = Seed Int

-- | Create a new seed from an integer.
mkSeed :: Int -> Seed
mkSeed = Seed . fromInt

-- | Generate a new seed using Javascript's PRNG.
newSeed :: IO Seed
newSeed = do
  x <- jsRand
  s <- jsRand
  let sign = if s > 0.5 then 1 else -1
  return . mkSeed . round_ $ x*sign*2147483647

-- | Generate the next seed in the sequence.
next :: Seed -> Seed
next (Seed s) =
  Seed s'
  where
    -- This is the same LCG that's used in older glibc versions.
    -- It was chosen because the untruncated product will never be larger than
    -- 2^53 and thus not cause precision problems with JS.
    a  = 69069
    c  = 1
    s' = a*s+c

class Random a where
  -- | Generate a pseudo random number between a lower (inclusive) and higher
  --   (exclusive) bound.
  randomR  :: (a, a) -> Seed -> (a, Seed)
  randomRs :: (a, a) -> Seed -> [a]
  randomRs bounds seed = unfoldr (Just . randomR bounds) seed

instance Random Int where
  randomR (low, high) s@(Seed n) =
    (n' `mod` (high-low) + low, next s)
    where
      -- Use the LCG from MSVC here; less apparent relationship between seed
      -- and output.
      a  = 214013
      c  = 2531011
      n' = a*n+c

instance Random Int32 where
  randomR (l,h) seed =
    case randomR (round_ l, round_ h) seed of
      (n, s) -> (fromInt n, s)

instance Random Word where
  randomR (l,h) seed =
    case randomR (round_ l, round_ h) seed of
      (n, s) -> (fromInt $ round_ n, s)

instance Random Word32 where
  randomR (l,h) seed =
    case randomR (round_ l, round_ h) seed of
      (n, s) -> (fromInt $ round_ n, s)

instance Random Double where
  randomR (low, high) seed =
    (f * (high-low) + low, s)
    where
      (n, s) = randomR (0, 2000000001 :: Int) seed
      f      = fromInt n / 2000000000
