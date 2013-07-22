{-# LANGUAGE MultiParamTypeClasses, ForeignFunctionInterface, MagicHash, 
             TypeSynonymInstances, FlexibleInstances, EmptyDataDecls,
             UnliftedFFITypes, UndecidableInstances #-}
-- | Efficient conversions to and from JS native types.
module Haste.JSType (
    JSType (..), JSNum (..), toString, fromString, convert
  ) where
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
instance JSType Int
instance JSType Int8
instance JSType Int16
instance JSType Int32
instance JSType Word
instance JSType Word8
instance JSType Word16
instance JSType Word32
instance JSType Float
instance JSType Double

-- This is completely insane, but GHC for some reason pukes when we try to
-- use the constructors of the actual integers, so we coerce them into this
-- isomorphic type to work with them instead.
data Dummy = Small Int# | Big ByteArray#

instance JSType Integer where
  toJSString n =
    case unsafeCoerce# n of
      Small n' -> toJSString (I# n')
      Big n'   -> jsIToString n'
  fromJSString s =
    case jsStringToI s of
      n -> Just (unsafeCoerce# (Big n))

instance JSType String where
  toJSString     = toJSStr
  fromJSString   = Just . fromJSStr

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