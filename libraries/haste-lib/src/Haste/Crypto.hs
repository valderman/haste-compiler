{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Haste.Crypto
  ( -- * Crypto-strength random number generation
    CryptoRandom (..)
  , cryptoRandomSalt
  , cryptoRandomUArray
  , cryptoRandomIOUArray

    -- * SubtleCrypto API. Presently only supports symmetric encryption, due to
    --   lack of browser support for other components.
  , Cipher (..), CipherMode (..), KeyLength (..), SymmetricKey, IV (..)
  , Salt (..), Password
  , generateKey, generateIV, keyFromBytes, getKeyBytes, deriveKey, keyCipher
  , encrypt, decrypt
  , encryptWithIV, decryptWithIV
  , encryptUArray, decryptUArray
  ) where
import Control.Monad
import Control.Monad.IO.Class
import Haste
import Haste.Binary
import Haste.Concurrent
import Haste.Foreign (toAny, fromAny)
import Data.Word
import Data.Int
import Data.Array.IO
import Data.Array.Unboxed
import System.IO.Unsafe

import Haste.Crypto.Types
import Haste.Crypto.Prim

-- crypto.getRandomBytes bindings

class CryptoRandom a where
  -- | Generate an element of type @a@ using the browser's crypto strength
  --   random bit generator.
  cryptoRandom :: MonadIO m => m a

  -- | Generate @n@ random elements of type @a@.
  cryptoRandoms :: MonadIO m => Word -> m [a]
  cryptoRandoms n = mapM (const cryptoRandom) [1..n]

instance ArrView a => CryptoRandom a where
  cryptoRandom = randomPrimitive
  cryptoRandoms = cryptoRandomIOUArray >=> liftIO . getElems

-- | Generate a random immutable array of @n@ elements.
cryptoRandomUArray :: (MonadIO m, ArrView a) => Word -> m (UArray Word a)
cryptoRandomUArray = cryptoRandomIOUArray >=> liftIO . unsafeFreeze

-- | Generate a random mutable array of @n@ elements.
cryptoRandomIOUArray :: (MonadIO m, ArrView a) => Word -> m (IOUArray Word a)
cryptoRandomIOUArray n = liftIO $ do
  a <- newArray_ (0, n-1)
  randomBits a
  return a

-- | Generate a random salt of the given length.
{-# INLINE cryptoRandomSalt #-}
cryptoRandomSalt :: MonadIO m => Word -> m Salt
cryptoRandomSalt = fmap Salt . cryptoRandomUArray

-- | Generate a random primitive using @window.crypto@.
randomPrimitive :: (MonadIO m, ArrView a) => m a
randomPrimitive = cryptoRandomIOUArray 1 >>= liftIO . flip readArray 0

-- | Freeze a mutable array into an immutable one. Only safe if the source
--   array is guaranteed to never be mutated post-freeze.
unsafeFreeze :: (Ix i, ArrView e) => IOUArray i e -> IO (UArray i e)
unsafeFreeze = pure . toAny >=> fromAny



-- Partial SubtleCrypto bindings

-- | Generate a key for the given cipher.
generateKey :: MonadConc m => Cipher -> m SymmetricKey
generateKey = promise . generateKey'

-- | Get the specified key as an unboxed byte array.
--   Only the bytes of the key itself are returned; exported keys do not
--   include information about which cipher they are for.
getKeyBytes :: MonadConc m => SymmetricKey -> m (UArray Word Word8)
getKeyBytes = promise . keyBytes'

-- | Import a key from an unboxed byte array.
keyFromBytes :: MonadConc m => Cipher -> UArray Word Word8 -> m (Either JSString SymmetricKey)
keyFromBytes c k = promiseE $ keyFromBytes' c k

-- | Encrypt a message using the given cipher, key and initialization vector.
encryptUArray :: (Ix i, ArrView e, MonadConc m)
        => SymmetricKey -> IV -> UArray i e -> m (UArray Word Word8)
encryptUArray k iv msg = promise (encrypt' k iv msg)

-- | Encrypt a message using the given cipher, key and initialization vector.
decryptUArray :: (Ix i, ArrView e, MonadConc m)
        => SymmetricKey -> IV -> UArray i e -> m (Either JSString (UArray Word Word8))
decryptUArray k iv msg = promiseE $ decrypt' k iv msg

-- | Encrypt a message using a custom initialization vector. The IV will
--   not be prepended to the output.
encryptWithIV :: (Binary a, MonadIO m, MonadBlob m, MonadConc m)
              => IV
              -> SymmetricKey
              -> a
              -> m Blob
encryptWithIV iv k val = do
  msg <- getBlobData $ encode val
  fromUArray <$> encryptUArray k iv (toUArray msg :: UArray Word Word8)

-- | Decrypt a message using a custom initialization vector.
decryptWithIV :: (Binary a, MonadIO m, MonadBlob m, MonadConc m)
              => IV
              -> SymmetricKey
              -> Blob
              -> m (Either JSString a)
decryptWithIV iv k blob = do
  msg <- getBlobData blob
  emsg' <- decryptUArray k iv (toUArray msg :: UArray Word Word8)
  case emsg' of
    Left err   -> return $ Left err
    Right msg' -> do
      eres <- decodeBlob $ fromUArray msg'
      case eres of
        Right res -> return $ Right res
        Left err  -> return $ Left $ toJSString err

-- | Generate an byte initialization vector of appropriate length for the
--   given cipher.
generateIV :: MonadIO m => Cipher -> m IV
generateIV = fmap IV . cryptoRandomUArray . ivLength

-- | The default initialization vector length for the available ciphers.
ivLength :: Cipher -> Word
ivLength (AES CBC Bits128) = 16
ivLength (AES CBC Bits256) = 32
ivLength (AES GCM _)       = 12

-- | Encrypt and encrypt a 'Binary' value using a random initialization vector.
--   The IV will be prepended to the message; when using GCM mode, the IV will
--   be 12 bytes, otherwise it will match the length of the key.
--
--   An unboxed array can be obtained from the
--   resulting 'Blob':
--
--       toUArray <$> getBlobData blob
encrypt :: (Binary a, MonadIO m, MonadBlob m, MonadConc m)
        => SymmetricKey
        -> a
        -> m Blob
encrypt k msg = do
  iv <- generateIV (keyCipher k)
  liftIO $ putStrLn $ "IV-enc: " ++ show (ivBytes iv)
  msg' <- encryptWithIV iv k msg
  let derp = runPut (putBlob (fromUArray (ivBytes iv)) >> putBlob msg')
  return derp

-- | Decrypt and decode a 'Binary' value from a 'Blob'. The initialization
--   vector will be read from the beginning of the message; see 'encrypt' for
--   information about IV lengths.
--   A blob can be obtained from an unboxed array using 'fromUArray'.
decrypt :: (Binary a, MonadIO m, MonadBlob m, MonadConc m)
        => SymmetricKey
        -> Blob
       -> m (Either JSString a)
decrypt k blob = do
  bd <- getBlobData blob
  case (getIV bd, getMsg bd) of
    (Left e, _)           -> return $ Left $ catJSStr "" ["couldn't read IV: ", e]
    (_, Left e)           -> return $ Left $ catJSStr "" ["couldn't read data: ", e]
    (Right iv, Right msg) -> do
      liftIO $ putStrLn $ "IV-dec: " ++ show (ivBytes $ IV $ toUArray iv)
      liftIO $ putStrLn $ "msg: " ++ show (ivBytes $ IV $ toUArray msg)
      emsg' <- decryptUArray k (IV $ toUArray iv) (toUArray msg :: UArray Word Word8)
      case emsg' of
        Left err   -> return $ Left $ catJSStr "" ["decryption failed: ", err]
        Right msg' -> do
          eres <- decodeBlob $ fromUArray msg'
          case eres of
            Left err  -> return $ Left $ catJSStr "" ["couldn't parse value: ", err]
            Right res -> return $ Right res
  where
    ivLen = fromIntegral $ ivLength (keyCipher k)
    getIV = runGet $ getBytes ivLen
    getMsg = runGet $ skip ivLen >> getBytes (blobSize blob - ivLen)

-- | Derive a key from the given password and salt, using the PBKDF2 key
--   derivation over SHA-256, with the given number of iterations.
--
--   One million iterations take roughly half a second on a modern laptop.
--   Presently not supported by IE/Edge.
deriveKey :: MonadConc m => Cipher -> Int -> Salt -> Password -> m SymmetricKey
deriveKey c n s pass = promise $ deriveKey' c s n (encodeUtf8 pass)

encodeUtf8 :: JSString -> UArray Word Word8
encodeUtf8 = unsafePerformIO . encodeUtf8'
