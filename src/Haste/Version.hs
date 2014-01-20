-- | Contains version information for Haste.
module Haste.Version (hasteVersion, ghcVersion, bootVersion, needsReboot,
                      BootVer (..), bootFile) where
import System.IO.Unsafe
import System.Directory
import System.FilePath ((</>))
import System.IO
import Data.Version
import Config (cProjectVersion)
import Haste.Environment (hasteDir)

hasteVersion :: Version
hasteVersion = Version [0, 2, 10] []

ghcVersion :: String
ghcVersion = cProjectVersion

bootVersion :: BootVer
bootVersion = BootVer hasteVersion ghcVersion

bootFile :: FilePath
bootFile = hasteDir </> "booted"

data BootVer = BootVer Version String deriving (Read, Show)

-- | Returns which parts of Haste need rebooting. A change in the boot file
--   format triggers a full reboot.
needsReboot :: Bool
needsReboot = unsafePerformIO $ do
  exists <- doesFileExist bootFile
  if exists
    then do
      fh <- openFile bootFile ReadMode
      bootedVerString <- hGetLine fh
      hClose fh
      case reads bootedVerString of
        [(BootVer hasteVer ghcVer, _)] ->
          return $ hasteVer /= hasteVersion || ghcVer /= ghcVersion
        _ ->
          return True
    else
      return True
