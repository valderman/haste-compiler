{-# LANGUAGE MagicHash, CPP, MultiParamTypeClasses, OverloadedStrings,
             TypeSynonymInstances , FlexibleInstances,
             GeneralizedNewtypeDeriving, BangPatterns, TypeOperators,
             KindSignatures, DefaultSignatures, FlexibleInstances,
             TypeSynonymInstances, FlexibleContexts, ScopedTypeVariables #-}
-- | Handling of Javascript-native binary blobs.
--
-- Generics borrowed from the binary package by Lennart Kolmodin (released under BSD3)
module Haste.Binary (
    module Haste.Binary.Put,
    module Haste.Binary.Get,
    MonadBlob (..), Binary (..), getBlobText,
    Blob, BlobData,
    blobSize, blobDataSize, toByteString, fromByteString, toBlob, strToBlob,
    encode, decode, decodeBlob
  )where
import Data.Int
import Data.Word
import Data.Char
import GHC.Fingerprint.Type
import qualified Haste.JSString as J (length)
import Haste.Prim
import Haste.Concurrent
import Haste.Prim.Foreign hiding (get)
import Haste.Binary.Types
import Haste.Binary.Put
import Haste.Binary.Get
import GHC.Generics
import Data.Bits
#ifndef __HASTE__
import qualified Data.ByteString.Lazy.Char8 as BS (unpack)
#endif

class Monad m => MonadBlob m where
  -- | Retrieve the raw data from a blob.
  getBlobData :: Blob -> m BlobData
  -- | Interpret a blob as UTF-8 text, as a JSString.
  getBlobText' :: Blob -> m JSString

-- | Interpret a blob as UTF-8 text.
getBlobText :: MonadBlob m => Blob -> m String
getBlobText b = getBlobText' b >>= return . fromJSStr

instance MonadBlob CIO where
#ifdef __HASTE__
  getBlobData b = do
      res <- newEmptyMVar
      liftIO $ convertBlob b (mkBlobData res (blobSize b))
      takeMVar res
    where
      mkBlobData res len x = concurrent $ do
        putMVar res (BlobData 0 len x)

      convertBlob :: Blob -> (JSAny -> IO ()) -> IO ()
      convertBlob = ffi
        "(function(b,cb){var r=new FileReader();r.onload=function(){cb(new DataView(r.result));};r.readAsArrayBuffer(b);})"

  getBlobText' b = do
      res <- newEmptyMVar
      liftIO $ convertBlob b (concurrent . putMVar res)
      takeMVar res
    where
      convertBlob :: Blob -> (JSString -> IO ()) -> IO ()
      convertBlob = ffi
        "(function(b,cb){var r=new FileReader();r.onload=function(){cb(r.result);};r.readAsText(b);})"
#else
  getBlobData (Blob b) = return (BlobData b)
  getBlobText' (Blob b) = return . toJSStr $ BS.unpack b
#endif

-- | Somewhat efficient serialization/deserialization to/from binary Blobs.
--   The layout of the binaries produced/read by get/put and encode/decode may
--   change between versions. If you need a stable binary format, you should
--   make your own using the primitives in Haste.Binary.Get/Put.
class Binary a where
  get :: Get a
  put :: a -> Put

  default put :: (Generic a, GBinary (Rep a)) => a -> Put
  put = gput . from

  default get :: (Generic a, GBinary (Rep a)) => Get a
  get = to `fmap` gget

-- | Generic version
class GBinary f where
    gput :: f t -> Put
    gget :: Get (f t)

instance Binary Bool where
  put True = putWord8 1
  put _    = putWord8 0
  get = do
    n <- getWord8
    case n of
      0 -> pure False
      1 -> pure True
      _ -> fail $ "Not a valid Bool: " ++ show n

instance Binary Word8 where
  put = putWord8
  get = getWord8

instance Binary Word16 where
  put = putWord16le
  get = getWord16le

instance Binary Word32 where
  put = putWord32le
  get = getWord32le

instance Binary Word64 where
  get = do
    lo <- get :: Get Word32
    hi <- get :: Get Word32
    return $ fromIntegral lo .|. shiftL (fromIntegral hi) 32
  put x = do
    put (fromIntegral x :: Word32)
    put (fromIntegral (shiftR x 32) :: Word32)

instance Binary Int8 where
  put = putInt8
  get = getInt8

instance Binary Int16 where
  put = putInt16le
  get = getInt16le

instance Binary Int32 where
  put = putInt32le
  get = getInt32le

instance Binary Int64 where
  get = do
    lo <- get :: Get Int32
    hi <- get :: Get Int32
    return $ fromIntegral lo .|. shiftL (fromIntegral hi) 32
  put x = do
    put (fromIntegral x :: Int32)
    put (fromIntegral (shiftR x 32) :: Int32)

instance Binary Int where
  put = putInt32le . fromIntegral
  get = fromIntegral <$> getInt32le

instance Binary Float where
  put = putFloat32le
  get = getFloat32le

instance Binary Double where
  put = putFloat64le
  get = getFloat64le

instance Binary Fingerprint where
  get = Fingerprint <$> get <*> get
  put (Fingerprint a b) = put a >> put b

instance (Binary a, Binary b) => Binary (a, b) where
  put (a, b) = put a >> put b
  get = do
    a <- get
    b <- get
    return (a, b)

instance Binary a => Binary (Maybe a) where
  put (Just x) = putWord8 1 >> put x
  put _        = putWord8 0
  get = do
    tag <- getWord8
    case tag of
      0 -> return Nothing
      1 -> Just <$> get
      _ -> fail "Wrong constructor tag when reading Maybe value!"

instance (Binary a, Binary b) => Binary (Either a b) where
  put (Left x)  = putWord8 0 >> put x
  put (Right x) = putWord8 1 >> put x
  get = do
    tag <- getWord8
    case tag of
      0 -> Left <$> get
      1 -> Right <$> get
      _ -> fail "Wrong constructor tag when reading Either value!"

instance Binary () where
  put _ = return ()
  get = return ()

instance Binary a => Binary [a] where
  put xs = do
    putWord32le (fromIntegral $ length xs)
    mapM_ put xs
  get = do
      len <- getWord32le
      getList len []
    where
      getList 0 xs = return $ reverse xs
      getList n xs = get >>= \x -> getList (n-1) (x:xs)

instance Binary JSString where
  {-# NOINLINE put #-}
  put s = do
    putWord32le $ fromIntegral $ J.length s
    putJSString s
  {-# NOINLINE get #-}
  get = get >>= getJSString
    
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
  get = get >>= \x ->
    case chr x of
      !x' -> return x'

-- | Encode any serializable data into a 'Blob'.
encode :: Binary a => a -> Blob
encode x = runPut (put x)

-- | Decode any deserializable data from a 'BlobData'.
decode :: Binary a => BlobData -> Either String a
decode = runGet get

-- | Decode a 'Blob' into some deserializable value, inconveniently locked up
--   inside the 'CIO' monad (or any other concurrent monad) due to the somewhat
--   special way JavaScript uses to deal with binary data.
decodeBlob :: (MonadBlob m, Binary a) => Blob -> m (Either String a)
decodeBlob b = getBlobData b >>= return . decode

-- Type without constructors
instance GBinary V1 where
    gput _ = return ()
    gget   = return undefined

-- Constructor without arguments
instance GBinary U1 where
    gput U1 = return ()
    gget    = return U1

-- Product: constructor with parameters
instance (GBinary a, GBinary b) => GBinary (a :*: b) where
    gput (x :*: y) = gput x >> gput y
    gget = (:*:) <$> gget <*> gget

-- Metadata (constructor name, etc)
instance GBinary a => GBinary (M1 i c a) where
    gput = gput . unM1
    gget = M1 <$> gget

-- Constants, additional parameters, and rank-1 recursion
instance Binary a => GBinary (K1 i a) where
    gput = put . unK1
    gget = K1 <$> get

-- Borrowed from the cereal package.

-- The following GBinary instance for sums has support for serializing
-- types with up to 2^64-1 constructors. It will use the minimal
-- number of bytes needed to encode the constructor. For example when
-- a type has 2^8 constructors or less it will use a single byte to
-- encode the constructor. If it has 2^16 constructors or less it will
-- use two bytes, and so on till 2^64-1.
--
-- NB: changed to 2^32-1 constructors

#define GUARD(WORD) (size - 1) <= fromIntegral (maxBound :: WORD)
#define PUTSUM(WORD) GUARD(WORD) = putSum (0 :: WORD) (fromIntegral size)
#define GETSUM(WORD) GUARD(WORD) = (get :: Get WORD) >>= checkGetSum (fromIntegral size)

instance ( GSum     a, GSum     b
         , GBinary a, GBinary b
         , SumSize    a, SumSize    b) => GBinary (a :+: b) where
    gput | PUTSUM(Word8) | PUTSUM(Word16) | PUTSUM(Word32) --  | PUTSUM(Word64)
         | otherwise = sizeError "encode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word32)
    {-# INLINE gput #-}

    gget | GETSUM(Word8) | GETSUM(Word16) | GETSUM(Word32) --  | GETSUM(Word64)
         | otherwise = sizeError "decode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word32)
    {-# INLINE gget #-}

sizeError :: Show size => String -> size -> error
sizeError s size =
    error $ "Can't " ++ s ++ " a type with " ++ show size ++ " constructors"

------------------------------------------------------------------------

checkGetSum :: (Ord word, Num word, Bits word, GSum f)
            => word -> word -> Get (f a)
checkGetSum size code | code < size = getSum code size
                      | otherwise   = fail "Unknown encoding for constructor"
{-# INLINE checkGetSum #-}

class GSum f where
    getSum :: (Ord word, Num word, Bits word) => word -> word -> Get (f a)
    putSum :: (Num w, Bits w, Binary w) => w -> w -> f a -> Put

instance (GSum a, GSum b, GBinary a, GBinary b) => GSum (a :+: b) where
    getSum !code !size | code < sizeL = L1 <$> getSum code           sizeL
                       | otherwise    = R1 <$> getSum (code - sizeL) sizeR
        where
          sizeL = size `shiftR` 1
          sizeR = size - sizeL
    {-# INLINE getSum #-}

    putSum !code !size s = case s of
                             L1 x -> putSum code           sizeL x
                             R1 x -> putSum (code + sizeL) sizeR x
        where
          sizeL = size `shiftR` 1
          sizeR = size - sizeL
    {-# INLINE putSum #-}

instance GBinary a => GSum (C1 c a) where
    getSum _ _ = gget
    {-# INLINE getSum #-}

    putSum !code _ x = put code *> gput x
    {-# INLINE putSum #-}

------------------------------------------------------------------------

class SumSize f where
    sumSize :: Tagged f Word32

newtype Tagged (s :: * -> *) b = Tagged {unTagged :: b}

instance (SumSize a, SumSize b) => SumSize (a :+: b) where
    sumSize = Tagged $ unTagged (sumSize :: Tagged a Word32) +
                       unTagged (sumSize :: Tagged b Word32)

instance SumSize (C1 c a) where
    sumSize = Tagged 1
