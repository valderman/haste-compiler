{-# LANGUAGE ForeignFunctionInterface #-}
module Haste.Random (Random (..)) where
import Haste.Prim

foreign import ccall jsRand :: IO Double
foreign import ccall "jsRand" jsRandF :: IO Float

class Random a where
  randomIO :: (a, a) -> IO a

instance Random Double where
  randomIO (low, high) = do
    rnd <- jsRand
    return $ rnd*(high-low) + low

instance Random Float where
  randomIO (low, high) = do
    rnd <- jsRandF
    return $ rnd*(high-low) + low

instance Random Int where
  randomIO (low, high) = do
    rnd <- randomIO (fromInt low, fromInt high :: Double)
    return $ round_ rnd
