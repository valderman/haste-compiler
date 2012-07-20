module BootVer (bootVer, needsReboot, BootVer (..), RebuildInfo (..)) where
import System.IO.Unsafe
import Control.Applicative
import System.Directory
import System.IO

-- | Describes the boot version of the code generator and standard library
--   respectively. When codegenVer changes, everything needs to be rebuilt
--   (or fetched). When libVer changes, only the FRP.Fursuit.* and Haste.*
--   modules need to be reinstalled.
data BootVer = BootVer {
    codegenVer :: Int,
    libVer     :: Int
  } deriving (Read, Show)

data RebuildInfo = Dont | Libs | Everything
  deriving (Read, Show, Eq, Ord)

-- | The current boot version; if Haste is built with a boot version greater
--   than the one specified in its boot file, it needs to be rebooted.
bootVer :: BootVer
bootVer = BootVer {
    codegenVer = 1,
    libVer     = 0
  }

-- | Returns which parts of Haste need rebooting. A change in the boot file
--   format triggers a full reboot.
needsReboot :: RebuildInfo
needsReboot = unsafePerformIO $ do
  bootfile <- (++ "/booted") <$> getAppUserDataDirectory "haste"
  exists <- doesFileExist bootfile
  if exists
    then do
      fh <- openFile bootfile ReadMode
      bootedVerString <- hGetLine fh
      hClose fh
      case reads bootedVerString of
        [(BootVer bootedCG bootedLib, _)]
          | codegenVer (bootVer) > bootedCG -> return Everything
          | libVer (bootVer) > bootedLib    -> return Libs
          | otherwise                       -> return Dont
        _ ->
          return Everything
    else
      return Everything
