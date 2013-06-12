{-# LANGUAGE MultiParamTypeClasses, ForeignFunctionInterface, MagicHash, 
             OverlappingInstances, TypeSynonymInstances, FlexibleInstances, 
             EmptyDataDecls, UnliftedFFITypes, UndecidableInstances #-}
-- | Efficient conversions to and from JS native types.
module Haste.JSType (JSType, JSNum, toString, fromString, convert) where
import GHC.Prim
import GHC.Int
import GHC.Word
import GHC.Integer.GMP.Internals
import GHC.Types (Int (..))
import Haste.Prim (JSString, toJSStr, fromJSStr)

foreign import ccall "Number" jsNumber        :: JSString -> Double
foreign import ccall "String" jsString        :: Double -> JSString
foreign import ccall jsRound     :: Double -> Int
foreign import ccall "I_toInt" jsIToInt       :: ByteArray# -> Int
foreign import ccall "I_toString" jsIToString :: ByteArray# -> JSString
foreign import ccall "I_fromString" jsStringToI :: JSString -> ByteArray#
foreign import ccall "I_fromNumber" jsNumToI  :: ByteArray# -> ByteArray#

data JSNumber

class JSType a where
  toJSString   :: a -> JSString
  toJSString = unsafeCoerce# jsString

  -- Default method is for Int types.
  fromJSString :: JSString -> Maybe a
  fromJSString s =
    case jsNumber s of
      d | isNaN d   -> Nothing
        | otherwise -> Just (unsafeCoerce# (jsRound d))

class JSNum a where
  toNumber   :: a -> JSNumber
  toNumber = unsafeCoerce#

  fromNumber :: JSNumber -> a
  fromNumber = unsafeCoerce#


-- JSNum instances

instance JSNum Int where
  fromNumber = unsafeCoerce# jsRound

instance JSNum Int8 where
  fromNumber n = case fromNumber n of I# n' -> I8# (narrow8Int# n')

instance JSNum Int16 where
  fromNumber n = case fromNumber n of I# n' -> I16# (narrow16Int# n')

instance JSNum Int32 where
  fromNumber = unsafeCoerce# jsRound

instance JSNum Word where
  fromNumber n =
    case jsRound (unsafeCoerce# n) of
      I# n' -> W# (int2Word# n')

instance JSNum Word8 where
  fromNumber w = case fromNumber w of W# w' -> W8# (narrow8Word# w')

instance JSNum Word16 where
  fromNumber w = case fromNumber w of W# w' -> W16# (narrow16Word# w')

instance JSNum Word32 where
  fromNumber w = case fromNumber w of W# w' -> W32# w'

instance JSNum Integer where
  toNumber (S# n) = toNumber (I# n)
  toNumber (J# n) = unsafeCoerce# (jsIToInt n)
  fromNumber n    = J# (jsNumToI (unsafeCoerce# n))

instance JSNum Float
instance JSNum Double


-- JSType instances

instance JSNum a => JSType a

instance JSType String where
  toJSString   = toJSStr
  fromJSString = Just . fromJSStr

instance JSType Integer where
  toJSString (S# n) = toJSString (I# n)
  toJSString (J# n) = jsIToString n
  fromJSString s    = Just (J# (jsStringToI s))

instance JSType () where
  toJSString _ = toJSStr "()"


-- Derived functions

toString :: JSType a => a -> String
toString = fromJSStr . toJSString

fromString :: JSType a => String -> Maybe a
fromString = fromJSString . toJSStr

convert :: (JSNum a, JSNum b) => a -> b
convert = fromNumber . toNumber

{-
TODO:
  JS-native round
-}