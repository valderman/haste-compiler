module HasteReboot (bootVer, needsReboot) where
import System.IO.Unsafe
import Control.Applicative
import System.Directory

-- | The current boot version; if Haste is built with a boot version greater
--   than the one specified in its boot file, it needs to be rebooted.
bootVer :: Int
bootVer = 0

-- | True if Haste needs rebooting, otherwise False.
needsReboot :: Bool
needsReboot = unsafePerformIO $ do
  bootfile <- (++ "/booted") <$> getAppUserDataDirectory "haste"
  exists <- doesFileExist bootfile
  if exists
    then do
      bootedVerString <- readFile bootfile
      case reads bootedVerString of
        [(bootedVer, _)] -> return $! bootVer > bootedVer
        _                -> return True
    else
      return True
