{-# LANGUAGE MagicHash, CPP, MultiParamTypeClasses, OverloadedStrings,
             TypeSynonymInstances , FlexibleInstances, OverlappingInstances,
             GeneralizedNewtypeDeriving #-}
-- | Handling of Javascript-native binary blobs.
module Haste.Binary (
    module Haste.Binary.Put,
    module Haste.Binary.Get,
    MonadBlob (..), Binary (..),
    Blob, BlobData,
    blobSize, blobDataSize, toByteString, toBlob, strToBlob,
    encode, decode
  )where
import Data.Int
import Data.Word
import Data.Char
import Haste
import Haste.Concurrent hiding (encode, decode)
import Haste.Foreign
import Haste.Binary.Types
import Haste.Binary.Put
import Haste.Binary.Get
import Control.Applicative

class Monad m => MonadBlob m where
  -- | Retrieve the raw data from a blob.
  getBlobData :: Blob -> m BlobData
  -- | Interpret a blob as UTF-8 text.
  getBlobText :: Blob -> m String

instance MonadBlob CIO where
  getBlobData b = do
      res <- newEmptyMVar
      liftIO $ convertBlob b (toOpaque $ mkBlobData res (blobSize b))
      takeMVar res
    where
#ifdef __HASTE__
      mkBlobData res len x = concurrent $ do
        putMVar res (BlobData 0 len x)
#else
      mkBlobData = undefined
#endif
      
      convertBlob :: Blob -> Opaque (Unpacked -> IO ()) -> IO ()
      convertBlob = ffi
        "(function(b,cb){var r=new FileReader();r.onload=function(){A(cb,[new DataView(r.result),0]);};r.readAsArrayBuffer(b);})"

  getBlobText b = do
      res <- newEmptyMVar
      liftIO $ convertBlob b (toOpaque $ concurrent . putMVar res)
      fromJSStr <$> takeMVar res
    where
      convertBlob :: Blob -> Opaque (JSString -> IO ()) -> IO ()
      convertBlob = ffi
        "(function(b,cb){var r=new FileReader();r.onload=function(){A(cb,[[0,r.result],0]);};r.readAsText(b);})"

class Binary a where
  get :: Get a
  put :: a -> Put

instance Binary Word8 where
  put = putWord8
  get = getWord8

instance Binary Word16 where
  put = putWord16le
  get = getWord16le

instance Binary Word32 where
  put = putWord32le
  get = getWord32le

instance Binary Int8 where
  put = putInt8
  get = getInt8

instance Binary Int16 where
  put = putInt16le
  get = getInt16le

instance Binary Int32 where
  put = putInt32le
  get = getInt32le

instance Binary Int where
  put = putInt32le . fromIntegral
  get = fromIntegral <$> getInt32le

instance Binary Float where
  put = putFloat32le
  get = getFloat32le

instance Binary Double where
  put = putFloat64le
  get = getFloat64le

instance Binary a => Binary [a] where
  put xs = do
    putWord32le (fromIntegral $ length xs)
    mapM_ put xs
  get = do
    len <- getWord32le
    flip mapM [1..len] $ \_ -> get

instance Binary Blob where
  {-# NOINLINE put #-}
  put b = do
    put (blobSize b)
    putBlob b
  {-# NOINLINE get #-}
  get = do
    sz <- get
    bd <- getBytes sz
    return $ toBlob bd

instance Binary Char where
  put = put . ord
  get = chr <$> get

encode :: Binary a => a -> Blob
encode x = runPut (put x)

decode :: Binary a => BlobData -> Either String a
decode = runGet get
