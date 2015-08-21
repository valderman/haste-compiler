{-# LANGUAGE CPP #-}
-- | Paths, host bitness and other environmental information about Haste.
module Haste.Environment (
    hasteSysDir, jsmodSysDir, pkgSysDir, pkgSysLibDir, jsDir,
    hasteUserDir, jsmodUserDir, pkgUserDir, pkgUserLibDir,
    hasteGhcLibDir,
    hostWordSize,
    ghcPkgBinary, ghcBinary,
    hasteBinDir, hasteBinary, hastePkgBinary, hasteInstHisBinary,
    hasteCabalBinary, hasteCopyPkgBinary,
    closureCompiler, bootFile,
    portableHaste, hasteNeedsReboot
  ) where
import System.IO.Unsafe
import Data.Bits
import Foreign.C.Types (CIntPtr)
import Control.Shell hiding (hClose)
import Paths_haste_compiler
import System.IO
import System.Info
import Haste.GHCPaths (ghcPkgBinary, ghcBinary)
import Haste.Version
#if defined(PORTABLE)
import System.Environment (getExecutablePath)
#endif

-- | Subdirectory under Haste's root directory where all the stuff for this
--   version lives.
--   Only x86-64 supported.
hasteVersionSubDir :: FilePath
hasteVersionSubDir = "x86_64-" ++ os ++ "-" ++ showBootVersion bootVersion

-- | Directory to search for GHC settings. Always equal to 'hasteSysDir'.
hasteGhcLibDir :: FilePath
hasteGhcLibDir = hasteSysDir

#if defined(PORTABLE)
-- | Was Haste built in portable mode or not?
portableHaste :: Bool
portableHaste = True

-- | Haste system directory. Identical to @hasteUserDir@ unless built with
--   -f portable.
hasteSysDir :: FilePath
hasteSysDir = dir </> hasteVersionSubDir
  where
    dir = joinPath . init . init . splitPath $ unsafePerformIO getExecutablePath

-- | Haste @bin@ directory.
hasteBinDir :: FilePath
hasteBinDir = takeDirectory $ unsafePerformIO getExecutablePath

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
    return $ d </> hasteVersionSubDir

-- | Directory where user .jsmod files are stored.
jsmodSysDir :: FilePath
jsmodSysDir = hasteSysDir

-- | Base directory for Haste's system libraries.
pkgSysLibDir :: FilePath
pkgSysLibDir = hasteSysDir

-- | Directory housing package information.
pkgSysDir :: FilePath
pkgSysDir = hasteSysDir </> "package.conf.d"

-- | Directory where user .jsmod files are stored.
jsmodUserDir :: FilePath
jsmodUserDir = hasteUserDir

-- | Directory containing library information.
pkgUserLibDir :: FilePath
pkgUserLibDir = hasteUserDir

-- | Directory housing package information.
pkgUserDir :: FilePath
pkgUserDir = hasteUserDir </> "package.conf.d"

-- | Host word size in bits.
hostWordSize :: Int
#if __GLASGOW_HASKELL__ >= 708
hostWordSize = finiteBitSize (undefined :: CIntPtr)
#else
hostWordSize = bitSize (undefined :: CIntPtr)
#endif

-- | File extension of binaries on this system.
binaryExt :: String
binaryExt
  | os == "mingw32" = ".exe"
  | otherwise       = ""

-- | The main Haste compiler binary.
hasteBinary :: FilePath
hasteBinary = hasteBinDir </> "hastec" ++ binaryExt

-- | Binary for haste-pkg.
hastePkgBinary :: FilePath
hastePkgBinary = hasteBinDir </> "haste-pkg" ++ binaryExt

-- | Binary for haste-copy-pkg.
hasteCopyPkgBinary :: FilePath
hasteCopyPkgBinary = hasteBinDir </> "haste-copy-pkg" ++ binaryExt

-- | Binary for haste-pkg.
hasteCabalBinary :: FilePath
hasteCabalBinary = hasteBinDir </> "haste-cabal" ++ binaryExt

-- | Binary for haste-install-his.
hasteInstHisBinary :: FilePath
hasteInstHisBinary = hasteBinDir </> "haste-install-his" ++ binaryExt

-- | JAR for Closure compiler.
closureCompiler :: FilePath
closureCompiler = hasteSysDir </> "compiler.jar"

-- | File indicating whether Haste is booted or not, and for which Haste+GHC
--   version combo.
bootFile :: FilePath
bootFile = hasteUserDir </> "booted"

-- | Returns which parts of Haste need rebooting. A change in the boot file
--   format triggers a full reboot.
hasteNeedsReboot :: Bool
#ifdef PORTABLE
Right hasteNeedsReboot = unsafePerformIO $ do
  shell $ not `fmap` isFile hasteCabalBinary
#else
hasteNeedsReboot = unsafePerformIO $ do
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
#endif
