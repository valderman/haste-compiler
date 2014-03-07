{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Haste.Binary.Types (
    Blob (..), BlobData (..),
    blobSize, blobDataSize, toByteString, toBlob, strToBlob
  ) where
import Haste.Prim
import Haste.Foreign
import System.IO.Unsafe
import qualified Data.ByteString.Lazy as BS
#ifndef __HASTE__
import qualified Data.ByteString.UTF8 as BU
#endif

#ifdef __HASTE__
data BlobData = BlobData Int Int Unpacked
newtype Blob = Blob Unpacked deriving Marshal

-- | The size, in bytes, of the contents of the given blob.
blobSize :: Blob -> Int
blobSize = unsafePerformIO . ffi "(function(b){return b.size;})"

-- | The size, in bytes, of the contents of the given blob data.
blobDataSize :: BlobData -> Int
blobDataSize (BlobData _ len _) = len

-- | Convert a BlobData to a ByteString. Only usable server-side.
toByteString :: BlobData -> BS.ByteString
toByteString =
  error "Haste.Binary.Types.toByteString called in browser context!"

-- | Convert a piece of BlobData back into a Blob.
toBlob :: BlobData -> Blob
toBlob (BlobData 0 len buf) =
  case newBlob buf of
    b | blobSize b > len -> sliceBlob b 0 len
      | otherwise        -> b
toBlob (BlobData off len buf) =
  sliceBlob (newBlob buf) off (off+len)

-- | Create a Blob from a JSString.
strToBlob :: JSString -> Blob
strToBlob = newBlob . unpack

sliceBlob :: Blob -> Int -> Int -> Blob
sliceBlob b off len = unsafePerformIO $ do
  ffi "(function(b,off,len){return b.slice(off,len);})" b off len

newBlob :: Unpacked -> Blob
newBlob = unsafePerformIO . ffi "(function(b){return new Blob([b]);})"
#else

newtype BlobData = BlobData BS.ByteString
newtype Blob = Blob BS.ByteString

-- Never used except for type checking
instance Marshal BlobData
instance Marshal Blob

-- | The size, in bytes, of the contents of the given blob.
blobSize :: Blob -> Int
blobSize (Blob b) = fromIntegral $ BS.length b

-- | The size, in bytes, of the contents of the given blob data.
blobDataSize :: BlobData -> Int
blobDataSize (BlobData bd) = fromIntegral $ BS.length bd

-- | Convert a BlobData to a ByteString. Only usable server-side.
toByteString :: BlobData -> BS.ByteString
toByteString (BlobData bd) = bd

-- | Convert a piece of BlobData back into a Blob.
toBlob :: BlobData -> Blob
toBlob (BlobData bs) = Blob bs

-- | Create a Blob from a JSString.
strToBlob :: JSString -> Blob
strToBlob s = Blob $ BS.fromChunks [BU.fromString $ fromJSStr s]

#endif
