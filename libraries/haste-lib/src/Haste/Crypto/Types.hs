{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Haste.Crypto.Types where
import Haste (JSString, JSType (..))
import Haste.Foreign
import Data.Array.Unboxed
import Data.Word

-- | Denotes a block cipher mode. As only CBC and GCM are universally supported
--   by browsers, these are presently the only modes exported by Haste.
--   If unsure, always use @GCM@.
data CipherMode = CBC | GCM
  deriving (Show, Read, Eq, Ord)

data KeyLength = Bits128 | Bits256
  deriving (Show, Read, Eq, Ord)

data Cipher = AES CipherMode KeyLength
  deriving (Show, Read, Eq, Ord)

-- | A key for a symmetric cipher.
data SymmetricKey = SymmetricKey JSAny Cipher

keyCipher :: SymmetricKey -> Cipher
keyCipher (SymmetricKey _ c) = c

instance ToAny SymmetricKey where
  toAny (SymmetricKey k c) = toObject [("key", k), ("cipher", toAny c)]

instance FromAny SymmetricKey where
  fromAny x = SymmetricKey <$> get x "key" <*> get x "cipher"

newtype IV = IV {ivBytes :: UArray Word Word8}
  deriving (ToAny, FromAny)

newtype Salt = Salt {saltBytes :: UArray Word Word8}
  deriving (ToAny, FromAny)

type Password = JSString

instance ToAny CipherMode where
  toAny = toAny . toJSString

instance ToAny KeyLength where
  toAny = toAny . toJSString

instance FromAny CipherMode where
  fromAny x = do
    s <- fromAny x
    maybe (fail "Unable to read CipherMode") return (fromJSString s)

instance FromAny KeyLength where
  fromAny x = do
    s <- fromAny x
    maybe (fail "Unable to read KeyLength") return (fromJSString s)

instance JSType KeyLength where
  toJSString Bits128 = "128"
  toJSString Bits256 = "256"
  fromJSString "128" = Just Bits128
  fromJSString "256" = Just Bits256
  fromJSString _     = Nothing

instance ToAny Cipher where
  toAny (AES mode len) = toObject
    [ ("name", toAny mode)
    , ("length", toAny len)
    ]

instance FromAny Cipher where
  fromAny x = AES <$> get x "name" <*> get x "length"

instance JSType CipherMode where
  toJSString CBC = "AES-CBC"
  toJSString GCM = "AES-GCM"

  fromJSString "AES-CBC" = Just CBC
  fromJSString "AES-GCM" = Just GCM
  fromJSString _         = Nothing

