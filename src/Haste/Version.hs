-- | Contains version information for Haste.
module Haste.Version (hasteVersion, ghcVersion, bootVersion, needsReboot,
                      BootVer (..), bootFile) where
import System.IO.Unsafe
import Control.Shell ((</>), shell, isFile, run)
import System.IO
import Data.Version
import Config (cProjectVersion)
import Haste.Environment (hasteSysDir, ghcBinary)

hasteVersion :: Version
hasteVersion = Version [0, 4, 3] []

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
