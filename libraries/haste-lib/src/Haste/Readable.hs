{-# LANGUAGE ForeignFunctionInterface, UnboxedTuples,
             MagicHash, TypeSynonymInstances, FlexibleInstances #-}
module Haste.Readable (Readable (..), read_) where
import Haste.Prim (toJSStr, round_, JSString)
import Unsafe.Coerce

foreign import ccall jsRead :: JSString -> Double

class Readable a where
  -- | Parse a String into a value.
  --   Must satisfy id == show_ . fromJust . readm.
  readm :: String -> Maybe a
  
  -- | Does the exact same thing as readm, but does not smack quotes on to
  --   parsed String values; that is, fromStr "hi" == "hi".
  fromStr :: String -> Maybe a
  fromStr = readm

instance Readable () where
  readm "()" = Just ()
  readm _    = Nothing

instance Readable String where
  readm s = Just $ '"' : s ++ "\""
  fromStr = Just

instance Readable Double where
  readm str =
    case jsRead (toJSStr str) of 
      d | isNaN d   -> Nothing
        | otherwise -> Just d

instance Readable Float where
  readm str = unsafeCoerce $ (readm str :: Maybe Double)

instance Readable Int where
  readm str = do
    d <- readm str :: Maybe Double
    let i = unsafeCoerce d
    if round_ d == i
       then return i
       else Nothing

instance Readable Integer where
  readm str =
    case readm str :: Maybe Double of
      Just d -> Just $ fromIntegral $ round_ d
      _      -> Nothing

instance Readable Bool where
  readm "True"  = Just True
  readm "False" = Just False
  readm _       = Nothing

instance Readable a => Readable (Maybe a) where
  readm ('J':'u':'s':'t':' ':str) = do
    val <- readm str
    return $ Just val
  readm "Nothing" =
    Just Nothing
  readm _ =
    Nothing

read_ :: Readable a => String -> a
read_ s =
  case readm s of
    Just x -> x
    _      -> error $ "Couldn't read: '" ++ s ++ "'"
