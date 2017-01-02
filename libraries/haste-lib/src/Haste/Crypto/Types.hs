{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Haste.Crypto.Types where
import Haste (JSString, JSType (..))
import Haste.Binary as B
import Haste.Foreign as F
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
  fromAny x = SymmetricKey <$> F.get x "key" <*> F.get x "cipher"

newtype IV = IV {ivBytes :: UArray Word32 Word8}
  deriving (ToAny, FromAny, Binary)

newtype Salt = Salt {saltBytes :: UArray Word32 Word8}
  deriving (ToAny, FromAny, Binary)

type Password = JSString

instance Binary CipherMode where
  put CBC = putWord8 0
  put GCM = putWord8 1
  get = do
    n <- getWord8
    case n of
      0 -> pure CBC
      1 -> pure GCM
      _ -> fail "Cipher mode was neither CBC nor GCM"

instance Binary KeyLength where
  put Bits128 = putWord8 0
  put Bits256 = putWord8 1
  get = do
    n <- getWord8
    case n of
      0 -> pure Bits128
      1 -> pure Bits256
      _ -> fail "Key length was neither 128 nor 256 bits"

instance Binary Cipher where
  put (AES mode len) = putWord8 0 >> put mode >> put len
  get = do
    n <- getWord8
    case n of
      0 -> AES <$> B.get <*> B.get
      _ -> fail "Invalid cipher"

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
  fromAny x = AES <$> F.get x "name" <*> F.get x "length"

instance JSType CipherMode where
  toJSString CBC = "AES-CBC"
  toJSString GCM = "AES-GCM"

  fromJSString "AES-CBC" = Just CBC
  fromJSString "AES-GCM" = Just GCM
  fromJSString _         = Nothing
