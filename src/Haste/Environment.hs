{-# LANGUAGE CPP #-}
-- | Paths, host bitness and other environmental information about Haste.
module Haste.Environment (
    hasteSysDir, jsmodSysDir, hasteInstSysDir, pkgSysDir, pkgSysLibDir, jsDir,
    hasteUserDir, jsmodUserDir, hasteInstUserDir, pkgUserDir, pkgUserLibDir,
    hostWordSize, ghcLibDir,
    ghcBinary, ghcPkgBinary,
    hasteBinary, hastePkgBinary, hasteInstHisBinary, hasteInstBinary,
    hasteCopyPkgBinary, closureCompiler, portableHaste,
    needsReboot, bootFile
  ) where
import System.IO.Unsafe
import Data.Bits
import Foreign.C.Types (CIntPtr)
import Control.Shell hiding (hClose)
import Paths_haste_compiler
import System.IO
import System.Environment (getExecutablePath)
import Haste.GHCPaths (ghcBinary, ghcPkgBinary, ghcLibDir)
import Haste.Version

#if defined(PORTABLE)
-- | Was Haste built in portable mode or not?
portableHaste :: Bool
portableHaste = True

-- | Haste system directory. Identical to @hasteUserDir@ unless built with
--   -f portable.
hasteSysDir :: FilePath
hasteSysDir =
  joinPath . init . init . splitPath $ unsafePerformIO getExecutablePath

-- | Haste @bin@ directory.
hasteBinDir :: FilePath
hasteBinDir = hasteSysDir </> "bin"

-- | Haste JS file directory.
jsDir :: FilePath
jsDir = hasteSysDir </> "js"

#else

-- | Was Haste built in portable mode or not?
portableHaste :: Bool
portableHaste = False

-- | Haste system directory. Identical to 'hasteUserDir' unless built with
--   -f portable.
hasteSysDir :: FilePath
hasteSysDir = hasteUserDir

-- | Haste @bin@ directory.
hasteBinDir :: FilePath
hasteBinDir = unsafePerformIO $ getBinDir

-- | Haste JS file directory.
jsDir :: FilePath
jsDir = unsafePerformIO $ getDataDir
#endif

-- | Haste user directory. Usually ~/.haste.
hasteUserDir :: FilePath
Right hasteUserDir =
  unsafePerformIO . shell . withAppDirectory "haste" $ \d -> do
    return $ d </> showBootVersion bootVersion

-- | Directory where user .jsmod files are stored.
jsmodSysDir :: FilePath
jsmodSysDir = hasteSysDir </> "jsmods"

-- | Base directory for haste-inst; system packages.
hasteInstSysDir :: FilePath
hasteInstSysDir = hasteSysDir </> "libraries"

-- | Base directory for Haste's system libraries.
pkgSysLibDir :: FilePath
pkgSysLibDir = hasteInstSysDir </> "lib"

-- | Directory housing package information.
pkgSysDir :: FilePath
pkgSysDir = hasteSysDir </> "packages"

-- | Directory where user .jsmod files are stored.
jsmodUserDir :: FilePath
jsmodUserDir = hasteUserDir </> "jsmods"

-- | Base directory for haste-inst.
hasteInstUserDir :: FilePath
hasteInstUserDir = hasteUserDir </> "libraries"

-- | Directory containing library information.
pkgUserLibDir :: FilePath
pkgUserLibDir = hasteInstUserDir </> "lib"

-- | Directory housing package information.
pkgUserDir :: FilePath
pkgUserDir = hasteUserDir </> "packages"

-- | Host word size in bits.
hostWordSize :: Int
#if __GLASGOW_HASKELL__ >= 708
hostWordSize = finiteBitSize (undefined :: CIntPtr)
#else
hostWordSize = bitSize (undefined :: CIntPtr)
#endif

-- | The main Haste compiler binary.
hasteBinary :: FilePath
hasteBinary = hasteBinDir </> "hastec"

-- | Binary for haste-pkg.
hastePkgBinary :: FilePath
hastePkgBinary = hasteBinDir </> "haste-pkg"

-- | Binary for haste-copy-pkg.
hasteCopyPkgBinary :: FilePath
hasteCopyPkgBinary = hasteBinDir </> "haste-copy-pkg"

-- | Binary for haste-pkg.
hasteInstBinary :: FilePath
hasteInstBinary = hasteBinDir </> "haste-inst"

-- | Binary for haste-install-his.
hasteInstHisBinary :: FilePath
hasteInstHisBinary = hasteBinDir </> "haste-install-his"

-- | JAR for Closure compiler.
closureCompiler :: FilePath
closureCompiler = hasteBinDir </> "compiler.jar"

-- | File indicating whether Haste is booted or not, and for which Haste+GHC
--   version combo.
bootFile :: FilePath
bootFile = hasteSysDir </> "booted"

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
      case parseBootVersion bootedVerString of
        Just (BootVer hasteVer ghcVer) ->
          return $ hasteVer /= hasteVersion || ghcVer /= ghcVersion
        _ ->
          return True
    _ -> do
      return True
