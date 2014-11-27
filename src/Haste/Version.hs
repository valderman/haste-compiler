-- | Contains version information for Haste.
module Haste.Version (
    BootVer (..),
    hasteVersion, intVersion, ghcVersion, bootVersion, needsReboot, bootFile
  ) where
import System.IO.Unsafe
import Control.Shell ((</>), shell, isFile, run)
import System.IO
import Data.Version
import Config (cProjectVersion)
import Haste.Environment (hasteSysDir, ghcBinary)

-- | Current Haste version.
hasteVersion :: Version
hasteVersion = Version [0, 5, 0] []

-- | Current Haste version as an Int. The format of this version number is
--   MAJOR*10 000 + MINOR*100 + MICRO.
--   Version 1.2.3 would thus be represented as 10203.
intVersion :: Int
intVersion = foldl (\a x -> a*100+x) 0 ver
  where Version ver _ = hasteVersion

-- | The version of GHC used to build this binary.
ghcVersion :: String
ghcVersion = unsafePerformIO $ do
  res <- shell $ run ghcBinary ["--numeric-version"] ""
  case res of
    Right ver -> return $ init ver -- remove trailing newline
    _         -> return cProjectVersion

bootVersion :: BootVer
bootVersion = BootVer hasteVersion ghcVersion

bootFile :: FilePath
bootFile = hasteSysDir </> "booted"

data BootVer = BootVer Version String deriving (Read, Show)

-- | Returns which parts of Haste need rebooting. A change in the boot file
--   format triggers a full reboot.
needsReboot :: Bool
needsReboot = unsafePerformIO $ do
  exists <- shell $ isFile bootFile
  case exists of
    Right True -> do
      fh <- openFile bootFile ReadMode
      bootedVerString <- hGetLine fh
      hClose fh
      case reads bootedVerString of
        [(BootVer hasteVer ghcVer, _)] ->
          return $ hasteVer /= hasteVersion || ghcVer /= ghcVersion
        _ ->
          return True
    _ -> do
      return True
