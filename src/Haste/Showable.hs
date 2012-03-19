{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances,
             UndecidableInstances #-}
module Haste.Showable (Showable (..)) where
import Unsafe.Coerce
import Haste.Prim

foreign import ccall jsShow   :: Double -> JSString

class Showable a where
  show_ :: a -> String

instance NumberRep a => Showable a where
  show_ = fromJSStr . jsShow . unsafeCoerce

instance Showable String where
  show_ xs = '"' : xs ++ "\""

instance Showable Bool where
  show_ True = "True"
  show_ False = "False"

instance Showable a => Showable (Maybe a) where
  show_ (Just x) = "Just " ++ show_ x
  show_ _        = "Nothing"
