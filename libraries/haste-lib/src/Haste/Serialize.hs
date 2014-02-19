{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
-- | JSON serialization and de-serialization for Haste.
module Haste.Serialize (
    Serialize (..), Parser, fromJSON, (.:), (.:?)
  ) where
import GHC.Float
import GHC.Int
import Haste.JSON
import Haste.Prim (JSString, toJSStr, fromJSStr)
import Control.Applicative
import Control.Monad (ap)

class Serialize a where
  toJSON :: a -> JSON

  listToJSON :: [a] -> JSON
  listToJSON = Arr . map toJSON

  parseJSON :: JSON -> Parser a

  parseJSONList :: JSON -> Parser [a]
  parseJSONList (Arr xs) = mapM parseJSON xs
  parseJSONList _        = fail "Tried to deserialie a non-array to a list!"

instance Serialize JSON where
  toJSON = id
  parseJSON = return

instance Serialize Float where
  toJSON = Num . float2Double
  parseJSON (Num x) = return (double2Float x)
  parseJSON _       = fail "Tried to deserialize a non-Number to a Float"

instance Serialize Double where
  toJSON = Num
  parseJSON (Num x) = return x
  parseJSON _       = fail "Tried to deserialize a non-Number to a Double"

instance Serialize Int where
  toJSON = Num . fromIntegral
  parseJSON (Num x) =
    case truncate x of
      x' | fromIntegral x' == x ->
        return x'
      _ ->
        fail "The given Number can't be represented as an Int"
  parseJSON _ =
    fail "Tried to deserialize a non-Number to an Int"

instance Serialize Int8 where
  toJSON = Num . fromIntegral
  parseJSON (Num x) =
    case truncate x of
      x' | x <= 0xff && fromIntegral x' == x ->
        return x'
      _ ->
        fail "The given Number can't be represented as an Int8"
  parseJSON _ =
    fail "Tried to deserialize a non-Number to an Int8"

instance Serialize Int16 where
  toJSON = Num . fromIntegral
  parseJSON (Num x) =
    case truncate x of
      x' | x <= 0xffff && fromIntegral x' == x ->
        return x'
      _ ->
        fail "The given Number can't be represented as an Int16"
  parseJSON _ =
    fail "Tried to deserialize a non-Number to an Int16"

instance Serialize Int32 where
  toJSON = Num . fromIntegral
  parseJSON (Num x) =
    case truncate x of
      x' | x < 0xffffffff && fromIntegral x' == x ->
        return x'
      _ ->
        fail "The given Number can't be represented as an Int32"
  parseJSON _ =
    fail "Tried to deserialize a non-Number to an Int32"

instance Serialize Bool where
  toJSON = Bool
  parseJSON (Bool x) = return x
  parseJSON _        = fail "Tried to deserialize a non-Bool to a Bool"

instance Serialize () where
  toJSON _ = Dict []
  parseJSON _ = return ()

instance Serialize Char where
  toJSON c = Str $ toJSStr [c]
  parseJSON (Str s) =
    case fromJSStr s of
      [c] -> return c
      _   -> fail "Tried to deserialize long string to a Char"
  parseJSON _ =
    fail "Tried to deserialize a non-string to a Char"
  listToJSON = toJSON . toJSStr
  parseJSONList s = fmap fromJSStr (parseJSON s)

instance Serialize JSString where
  toJSON = Str
  parseJSON (Str s) = return s
  parseJSON _ = fail "Tried to deserialize a non-JSString to a JSString"

instance (Serialize a, Serialize b) => Serialize (a, b) where
  toJSON (a, b) = Arr [toJSON a, toJSON b]
  parseJSON (Arr [a, b]) = do
    a' <- parseJSON a
    b' <- parseJSON b
    return (a', b')
  parseJSON _ =
    fail "Tried to deserialize a non-array into a pair!"

instance Serialize a => Serialize (Maybe a) where
  toJSON (Just x)  = Dict [("hasValue", toJSON True), ("value", toJSON x)]
  toJSON (Nothing) = Dict [("hasValue", toJSON False)]
  parseJSON d = do
    hasVal <- d .: "hasValue"
    case hasVal of
      False -> return Nothing
      _     -> Just `fmap` (d .: "value")

instance Serialize a => Serialize [a] where
  toJSON = listToJSON
  parseJSON = parseJSONList

instance (Serialize a, Serialize b) => Serialize (Either a b) where
  toJSON (Right x) = Dict [("success", toJSON True), ("value", toJSON x)]
  toJSON (Left e)  = Dict [("success", toJSON False), ("error", toJSON e)]
  parseJSON d = do
    success <- d .: "success"
    case success of
      False -> Left `fmap` (d .: "error")
      _     -> Right `fmap` (d .: "value")

fromJSON :: Serialize a => JSON -> Either String a
fromJSON = runParser parseJSON

-- | Type for JSON parser.
newtype Parser a = Parser (Either String a)

runParser :: (a -> Parser b) -> a -> Either String b
runParser p x = case p x of Parser y -> y

instance Monad Parser where
  return = Parser . return
  (Parser (Right x)) >>= f = f x
  (Parser (Left e))  >>= _ = Parser (Left e)
  fail = Parser . Left

instance Functor Parser where
  fmap f m = m >>= return . f

instance Applicative Parser where
  (<*>) = ap
  pure  = return

-- | Look up a key in a JSON object.
(.:) :: Serialize a => JSON -> JSString -> Parser a
Dict o .: key =
  case lookup key o of
    Just x -> parseJSON x
    _      -> Parser $ Left "Key not found"
_ .: _ =
  Parser $ Left "Tried to do lookup on non-object!"

(.:?) :: Serialize a => JSON -> JSString -> Parser (Maybe a)
o .:? key =
  case o .: key of
    Parser (Right x) -> return (Just x)
    _                -> return Nothing
