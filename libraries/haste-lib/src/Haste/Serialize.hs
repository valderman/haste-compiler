-- | JSON serialization and de-serialization for Haste.
module Haste.Serialize where
import GHC.Float
import GHC.Int
import Haste.JSON

class Serialize a where
  toJSON   :: a -> JSON
  fromJSON :: JSON -> Either String a

instance Serialize Float where
  toJSON = Num . float2Double
  fromJSON (Num x) = Right (double2Float x)
  fromJSON _       = Left "Tried to deserialize a non-Number to a Float"

instance Serialize Double where
  toJSON = Num
  fromJSON (Num x) = Right x
  fromJSON _       = Left "Tried to deserialize a non-Number to a Double"

instance Serialize Int where
  toJSON = Num . fromIntegral
  fromJSON (Num x) =
    case truncate x of
      x' | fromIntegral x' == x ->
        Right x'
      _ ->
        Left "The given Number can't be represented as an Int"
  fromJSON _ =
    Left "Tried to deserialize a non-Number to an Int"

instance Serialize Int8 where
  toJSON = Num . fromIntegral
  fromJSON (Num x) =
    case truncate x of
      x' | x <= 0xff && fromIntegral x' == x ->
        Right x'
      _ ->
        Left "The given Number can't be represented as an Int8"
  fromJSON _ =
    Left "Tried to deserialize a non-Number to an Int8"

instance Serialize Int16 where
  toJSON = Num . fromIntegral
  fromJSON (Num x) =
    case truncate x of
      x' | x <= 0xffff && fromIntegral x' == x ->
        Right x'
      _ ->
        Left "The given Number can't be represented as an Int16"
  fromJSON _ =
    Left "Tried to deserialize a non-Number to an Int16"

instance Serialize Int32 where
  toJSON = Num . fromIntegral
  fromJSON (Num x) =
    case truncate x of
      x' | x < 0xffffffff && fromIntegral x' == x ->
        Right x'
      _ ->
        Left "The given Number can't be represented as an Int32"
  fromJSON _ =
    Left "Tried to deserialize a non-Number to an Int32"

instance Serialize Bool where
  toJSON = Bool
  fromJSON (Bool x) = Right x
  fromJSON _        = Left "Tried to deserialize a non-Bool to a Bool"
