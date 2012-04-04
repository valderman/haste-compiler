{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances,
             UndecidableInstances #-}
module Haste.Showable (Showable (..)) where
import Haste.Prim

foreign import ccall "jsShow" jsShowD :: Double -> JSString
foreign import ccall "jsShow" jsShowF :: Float -> JSString
foreign import ccall jsShowI :: Int -> JSString

class Showable a where
  -- | Equivalent to show; should satisfy id == show . read
  show_ :: a -> String
  
  -- | Equivalent to show_, except it should prioritize nice looking output;
  --   the identity id == show . read need not hold for instances.
  --   Implementing it in your instances is nearly always unnecessary.
  toStr :: a -> String
  toStr = show_

instance Showable Double where
  show_ = fromJSStr . jsShowD

instance Showable Float where
  show_ = fromJSStr . jsShowF

instance Showable Int where
  show_ = fromJSStr . jsShowI

instance Showable Integer where
  show_ n = show n

instance Showable String where
  show_ xs = '"' : xs ++ "\""
  toStr    = id

instance Showable JSString where
  show_ = show_ . fromJSStr
  toStr = toStr . fromJSStr

instance Showable Bool where
  show_ True  = "True"
  show_ False = "False"

instance Showable a => Showable (Maybe a) where
  show_ (Just x) = "Just " ++ show_ x
  show_ _        = "Nothing"
