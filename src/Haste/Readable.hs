{-# LANGUAGE ForeignFunctionInterface, UnboxedTuples, MagicHash #-}
module Haste.Readable where
import Haste.Prim (toJSStr, round_, JSString)
import Unsafe.Coerce

foreign import ccall isNull :: Double -> Bool
foreign import ccall jsRead :: JSString -> Double

class Readable a where
  fromString :: String -> Maybe a

instance Readable Double where
  fromString str =
    case jsRead (toJSStr str) of 
      d | isNaN d   -> Nothing
        | otherwise -> Just d

instance Readable Float where
  fromString str = unsafeCoerce $ (fromString str :: Maybe Double)

instance Readable Int where
  fromString str = do
    d <- fromString str :: Maybe Double
    let i = unsafeCoerce d
    if round_ d == i
       then return i
       else Nothing

instance Readable Integer where
  fromString str =
    case fromString str :: Maybe Double of
      Just d -> Just $ round d
      _      -> Nothing

instance Readable Bool where
  fromString "True"  = Just True
  fromString "False" = Just False
  fromString _       = Nothing

instance Readable a => Readable (Maybe a) where
  fromString ('J':'u':'s':'t':' ':str) = do
    val <- fromString str
    return $ Just val
  fromString "Nothing" =
    Just Nothing
  fromString _ =
    Nothing

read_ :: Readable a => String -> a
read_ s = case fromString s of Just x -> x
