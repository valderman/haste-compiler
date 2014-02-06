{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
-- | JSON serialization and de-serialization for Haste.
module Haste.Serialize where
import GHC.Float
import GHC.Int
import Haste.JSON
import Haste.Prim (JSString, toJSStr, fromJSStr)

class Serialize a where
  toJSON :: a -> JSON

  listToJSON :: [a] -> JSON
  listToJSON = Arr . map toJSON

  fromJSON :: JSON -> Either String a

  listFromJSON :: JSON -> Either String [a]
  listFromJSON (Arr xs) = mapM fromJSON xs
  listFromJSON _        = Left "Tried to deserialie a non-array to a list!"

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

instance Serialize () where
  toJSON _ = Dict []
  fromJSON _ = Right ()

instance Serialize Char where
  toJSON c = Str $ toJSStr [c]
  fromJSON (Str s) =
    case fromJSStr s of
      [c] -> return c
      _   -> fail "Tried to deserialize long string to a Char"
  fromJSON _ =
    fail "Tried to deserialize a non-string to a Char"
  listToJSON = toJSON . toJSStr
  listFromJSON s = fmap fromJSStr (fromJSON s)

instance Serialize JSString where
  toJSON = Str
  fromJSON (Str s) = Right s
  fromJSON _ = Left "Tried to deserialize a non-JSString to a JSString"

instance (Serialize a, Serialize b) => Serialize (a, b) where
  toJSON (a, b) = Arr [toJSON a, toJSON b]
  fromJSON (Arr [a, b]) = do
    a' <- fromJSON a
    b' <- fromJSON b
    return (a', b')
  fromJSON _ =
    fail "Tried to deserialize a non-array into a pair!"

instance Serialize a => Serialize (Maybe a) where
  toJSON (Just x)  = Dict [("hasValue", toJSON True), ("value", toJSON x)]
  toJSON (Nothing) = Dict [("hasValue", toJSON False)]
  fromJSON d = do
    hasVal <- (d ~> "hasValue") >>= fromJSON
    case hasVal of
      False -> return Nothing
      _     -> Just `fmap` ((d ~> "value") >>= fromJSON)

instance Serialize a => Serialize [a] where
  toJSON = listToJSON
  fromJSON = listFromJSON
