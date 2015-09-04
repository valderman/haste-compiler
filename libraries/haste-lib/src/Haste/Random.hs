{-# LANGUAGE GeneralizedNewtypeDeriving, CPP, OverloadedStrings #-}
module Haste.Random (Random (..), Seed, next, mkSeed, newSeed) where
import Haste.Prim.JSType
import Data.Int
import Data.Word
import Data.List (unfoldr)
import Control.Monad.IO.Class
import System.IO.Unsafe
#ifdef __HASTE__
import Haste.Foreign
#else
import qualified System.Random as SR
#endif

#ifdef __HASTE__

newtype Seed = Seed JSAny deriving (ToAny, FromAny)

nxt :: Seed -> IO Seed
nxt = ffi "(function(s){\
var ba = window['newByteArr'](16);\
ba['v']['w32'][0] = s[0];\
ba['v']['w32'][1] = s[1];\
ba['v']['w32'][2] = s[2];\
ba['v']['w32'][3] = s[3];\
return window['md51'](ba,16);})"

getN :: Seed -> IO Int
getN = ffi "(function(s){return s[0];})"

toSeed :: Int -> IO Seed
toSeed = ffi "(function(n){\
var ba = window['newByteArr'](16);\
ba['v']['w32'][0] = n;\
return window['md51'](ba,16);})"

createSeed :: IO Seed
createSeed = ffi "(function(){\
var ba = window['newByteArr'](16);\
ba['v']['f64'][0] = Math.random();\
ba['v']['f64'][1] = Math.random();\
return window['md51'](ba,16);})"
#else
newtype Seed = Seed (Int, SR.StdGen)

nxt :: Seed -> IO Seed
nxt (Seed (_, g)) = return . Seed $ SR.next g

getN :: Seed -> IO Int
getN (Seed (n, _)) = return n

toSeed :: Int -> IO Seed
toSeed = return . Seed . SR.next . SR.mkStdGen

createSeed :: IO Seed
createSeed = SR.newStdGen >>= return . Seed . SR.next
#endif

-- | Create a new seed from an integer.
mkSeed :: Int -> Seed
mkSeed = unsafePerformIO . toSeed

-- | Generate a new seed using JavaScript's PRNG.
newSeed :: MonadIO m => m Seed
newSeed = liftIO createSeed

-- | Generate the next seed in the sequence.
next :: Seed -> Seed
next = unsafePerformIO . nxt

class Random a where
  -- | Generate a pseudo random number between a lower (inclusive) and higher
  --   (exclusive) bound.
  randomR  :: (a, a) -> Seed -> (a, Seed)
  randomRs :: (a, a) -> Seed -> [a]
  randomRs bounds seed = unfoldr (Just . randomR bounds) seed

instance Random Int where
  randomR (low, high) s
    | low <= high =
      let n = unsafePerformIO $ getN s
      in  (n `mod` (high-low+1) + low, next s)
    | otherwise =
      randomR (high, low) s

instance Random Int32 where
  randomR (l,h) seed =
    case randomR (convert l :: Int, convert h) seed of
      (n, s) -> (convert n, s)

instance Random Word where
  randomR (l,h) seed =
    case randomR (convert l :: Int, convert h) seed of
      (n, s) -> (convert n, s)

instance Random Word32 where
  randomR (l,h) seed =
    case randomR (convert l :: Int, convert h) seed of
      (n, s) -> (convert  n, s)

instance Random Double where
  randomR (low, high) seed =
    (f * (high-low) + low, s)
    where
      (n, s) = randomR (0, 2000000001 :: Int) seed
      f      = convert n / 2000000000
