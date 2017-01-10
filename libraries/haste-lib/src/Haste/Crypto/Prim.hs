{-# LANGUAGE OverloadedStrings #-}
module Haste.Crypto.Prim where
import Haste
import Haste.Foreign
import Haste.Crypto.Types
import Data.Array.Unboxed
import Data.Array.IO
import Data.Word
import Haste.Concurrent

-- | Fill the given array with random bits, using the browser's @window.crypto@
--   object.
randomBits :: ArrView e => IOUArray Word32 e -> IO ()
randomBits = ffi "(function(arr){window['__haste_crypto'].getRandomValues(arr);})"

-- | Wait for a promise to either complete or get rejected. Promises used with
--   this function should not get rejected unless there is a bug in this very
--   library.
--   Promises that may fail for normal reasons should not use this function.
promise :: MonadConc m
        => ((a -> IO ()) -> (JSString -> IO ()) -> IO ())
        -> m a
promise f = liftConc $ do
  res <- promiseE f
  case res of
    Right x -> return x
    Left e  -> error ("promise rejected: " ++ fromJSStr e)

-- | A promise that might fail.
promiseE :: MonadConc m
        => ((a -> IO ()) -> (JSString -> IO ()) -> IO ())
        -> m (Either JSString a)
promiseE f = liftConc $ do
  v <- newEmptyMVar
  liftIO $ f (concurrent . putMVar v . Right) (concurrent . putMVar v . Left)
  takeMVar v

generateKey' :: Cipher -> (SymmetricKey -> IO ()) -> (JSString -> IO ()) -> IO ()
generateKey' = ffi "(function(c, yay, nay) {\
  \window['__haste_crypto'].subtle.generateKey(c, true, ['encrypt','decrypt'])\
  \.then(function(k) {yay({key: k, cipher: c});})\
  \.catch(nay);\
  \})"

encrypt' :: (Ix i, ArrView e)
         => SymmetricKey -> IV -> UArray i e
         -> (UArray Word32 Word8 -> IO ())
         -> (JSString -> IO ())
         -> IO ()
encrypt' = ffi "(function(k, iv, data, yay, nay) {\
  \k.cipher.iv = iv;\
  \window['__haste_crypto'].subtle.encrypt(k.cipher, k.key, data)\
  \.then(function(enc){yay(new Uint8Array(enc));})\
  \.catch(nay);\
  \})"

decrypt' :: (Ix i, ArrView e)
         => SymmetricKey -> IV -> UArray i e
         -> (UArray Word32 Word8 -> IO ())
         -> (JSString -> IO ())
         -> IO ()
decrypt' = ffi "(function(k, iv, data, yay, nay) {\
  \k.cipher.iv = iv;\
  \window['__haste_crypto'].subtle.decrypt(k.cipher, k.key, data)\
  \.then(function(dec){yay(new Uint8Array(dec));})\
  \.catch(nay);\
  \})"

keyBytes' :: SymmetricKey
          -> (UArray Word32 Word8 -> IO ())
          -> (JSString -> IO ())
          -> IO ()
keyBytes' = ffi "(function(k, yay, nay){\
  \window['__haste_crypto'].subtle.exportKey('raw', k.key)\
  \.then(function(dec){yay(new Uint8Array(dec));})\
  \.catch(nay);\
  \})"

keyFromBytes' :: Cipher
              -> UArray Word32 Word8
              -> (SymmetricKey -> IO ())
              -> (JSString -> IO ())
              -> IO ()
keyFromBytes' = ffi "(function(alg, k, yay, nay){\
  \window['__haste_crypto'].subtle.importKey('raw', k, alg, true, ['encrypt', 'decrypt'])\
  \.then(function(k) {yay({key: k, cipher: alg});})\
  \.catch(nay);\
  \})"

deriveKey' :: Cipher -> Salt -> Int -> UArray Word32 Word8 -> (Maybe SymmetricKey -> IO ()) -> (JSString -> IO ()) -> IO ()
deriveKey' = ffi "(function(alg, s, n, k, yay, nay){\
  \window['__haste_crypto'].subtle.importKey('raw', k, {name:'PBKDF2'}, false, ['deriveKey']).then(function(mk) {\
  \window['__haste_crypto'].subtle.deriveKey({name:'PBKDF2',salt:s,iterations:n,hash:'SHA-256'}, mk, alg, true, ['encrypt', 'decrypt']).then(function(k){yay({key: k, cipher: alg});}).catch(function(_){yay(null);});\
  \});})"

encodeUtf8' :: JSString -> IO (UArray Word32 Word8)
encodeUtf8' = ffi "(function(s){\
  \s = unescape(encodeURIComponent(s));\
  \var arr = new Uint8Array(s.length);\
  \for(var i = 0 ; i < s.length; ++i) {\
  \  arr[i] = s.charCodeAt(i);\
  \}\
  \return arr;\
  \})"
