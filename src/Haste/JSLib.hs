{-# LANGUAGE RecordWildCards #-}
-- | JSLib file format.
module Haste.JSLib (createJSLib, readModule) where
import Control.Applicative
import Control.Monad
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import Haste.AST
import System.Directory (doesFileExist)
import System.IO

jslibMagicNumber :: Word64
jslibMagicNumber = 0x42494c534a -- "JSLIB\0\0\0"

jslibCurrentVersion :: Word8
jslibCurrentVersion = 0

jslibMaxSize :: Integer
jslibMaxSize = 2^(32 :: Int)-1

-- | Static size, ever unchanging part of the file format.
data JSLibStaticHdr = JSLibStaticHdr {
    hdrMagicNumber :: !Word64, -- ^ Always "JSLIB\0\0\0".
    hdrVersion     :: !Word8,  -- ^ JSLib format version.
    hdrSize        :: !Word32  -- ^ Size of the rest of the header, in bytes.
  }

-- | Library metadata: package ID and info on how to find the actual data.
data JSLibMetadata = JSLibMetadata {
    mdPkgId       :: !BS.ByteString, -- ^ Library's package ID.
    mdModules     :: ![(BS.ByteString, (Word32, Word32))]
                                     -- ^ Offsets and sizes of all modules in
                                     --   this lib, offsets starting from the
                                     --   end of the header.
  }

data JSLibHdr = JSLibHdr {
    hdrStaticHdr :: JSLibStaticHdr,
    hdrMetadata  :: JSLibMetadata
  }

instance Binary JSLibStaticHdr where
  put JSLibStaticHdr {..} = do
    put hdrMagicNumber
    put hdrVersion
    put hdrSize
  get = do
    JSLibStaticHdr <$> get <*> get <*> get

instance Binary JSLibMetadata where
  put JSLibMetadata {..} = put mdPkgId >> put mdModules
  get = JSLibMetadata <$> get <*> get

instance Binary JSLibHdr where
  put JSLibHdr {..} = put hdrStaticHdr >> put hdrMetadata
  get = JSLibHdr <$> get <*> get

-- | Create a jslib file by the given name, for the package with the given
--   package key, containing the modules described by the given list of files.
createJSLib :: FilePath -> BS.ByteString -> [FilePath] -> IO ()
createJSLib jslibfile pkgid jsmodfiles = do
  mods <- genModOffList 0 jsmodfiles
  let hdr = JSLibHdr {
          hdrStaticHdr = JSLibStaticHdr {
              hdrMagicNumber = jslibMagicNumber,
              hdrVersion = jslibCurrentVersion,
              hdrSize = fromIntegral . BSL.length $ encode (hdrMetadata hdr)
            },
          hdrMetadata = JSLibMetadata {
              mdPkgId = pkgid,
              mdModules = mods
            }
        }
  BSL.writeFile jslibfile $ encode hdr
  mapM_ (BSL.readFile >=> BSL.appendFile jslibfile) jsmodfiles
  where
    genModOffList off (f:fs) = do
      when (off > jslibMaxSize) $ error "jslib became too large!"
      (nm, sz) <- withBinaryFile f ReadMode $ \h -> do
        sz <- hFileSize h
        m <- decode `fmap` BSL.hGetContents h
        when (modPackageId m /= pkgid) $
          error $  "Module package key mismatch: expected " ++ BS.toString pkgid
                ++ " but saw " ++ BS.toString (modPackageId m)
        return (modName m, sz)

      ms <- genModOffList (off + sz) fs
      return $ (nm, (fromIntegral off, fromIntegral sz)) : ms
    genModOffList _ _ = do
      return []

-- | Read a module from a jslib file. Returns @Nothing@ if the jslib file does
--   not exist or does not contain the requested module.
readModule :: FilePath -> String -> IO (Maybe Module)
readModule pkgfile modname = do
  exists <- doesFileExist pkgfile
  if exists
    then withBinaryFile pkgfile ReadMode $ \h -> do
      magic <- decode <$> BSL.hGet h 8
      when (magic /= jslibMagicNumber) $ error "Bad jslib magic number!"
      version <- decode <$> BSL.hGet h 1
      when (version > jslibCurrentVersion) $ error "Bad jslib format version!"
      size <- decode <$> BSL.hGet h 4
      md <- decode <$> BSL.hGet h (fromIntegral (size :: Word32))
      case lookup (BS.fromString modname) $ mdModules md of
        Just (off, sz) -> do
          hSeek h RelativeSeek (fromIntegral off)
          Just . decode <$> BSL.hGet h (fromIntegral sz)
        _ -> do
          return Nothing
    else return Nothing
