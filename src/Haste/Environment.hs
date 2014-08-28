{-# LANGUAGE CPP #-}
-- | Paths, host bitness and other environmental information about Haste.
module Haste.Environment (
  hasteSysDir, jsmodSysDir, hasteInstSysDir, pkgSysDir, pkgSysLibDir, jsDir,
  hasteUserDir, jsmodUserDir, hasteInstUserDir, pkgUserDir, pkgUserLibDir,
  hostWordSize, ghcLibDir,
  ghcBinary, ghcPkgBinary,
  hasteBinary, hastePkgBinary, hasteInstHisBinary, hasteInstBinary,
  hasteCopyPkgBinary, closureCompiler, portableHaste) where
import System.IO.Unsafe
import Data.Bits
import Foreign.C.Types (CIntPtr)
import Control.Shell
import System.Environment (getExecutablePath)
import System.Directory (findExecutable)
import Paths_haste_compiler
import GHC.Paths (libdir)
import Config (cProjectVersion)
import Data.Maybe (catMaybes)

#if defined(PORTABLE)
portableHaste :: Bool
portableHaste = True

-- | Haste system directory. Identical to @hasteUserDir@ unless built with
--   -f portable.
hasteSysDir :: FilePath
hasteSysDir =
  joinPath . init . init . splitPath $ unsafePerformIO getExecutablePath

ghcLibDir :: FilePath
ghcLibDir = unsafePerformIO $ do
  Right out <- shell $ run ghcBinary ["--print-libdir"] ""
  return $ init out

hasteBinDir :: FilePath
hasteBinDir = hasteSysDir </> "bin"

jsDir :: FilePath
jsDir = hasteSysDir </> "js"
#else
portableHaste :: Bool
portableHaste = False

-- | Haste system directory. Identical to @hasteUserDir@ unless built with
--   -f portable.
hasteSysDir :: FilePath
hasteSysDir = hasteUserDir

ghcLibDir :: FilePath
ghcLibDir = libdir

hasteBinDir :: FilePath
hasteBinDir = unsafePerformIO $ getBinDir

jsDir :: FilePath
jsDir = unsafePerformIO $ getDataDir
#endif

-- | Haste user directory. Usually ~/.haste.
hasteUserDir :: FilePath
Right hasteUserDir = unsafePerformIO . shell $ withAppDirectory "haste" return

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

-- | Path to the GHC binary.
ghcBinary :: FilePath
ghcBinary = unsafePerformIO $ do
  exes <- catMaybes `fmap` mapM findExecutable ["ghc-" ++ cProjectVersion,
                                                "ghc"]
  case exes of
    (exe:_) -> return exe
    _       -> error $  "No appropriate GHC executable in search path!\n"
                     ++ "Are you sure you have GHC " ++ cProjectVersion
                     ++ " installed?"

-- | Path to the GHC binary.
ghcPkgBinary :: FilePath
ghcPkgBinary = unsafePerformIO $ do
  exes <- catMaybes `fmap` mapM findExecutable ["ghc-pkg-" ++ cProjectVersion,
                                                "ghc-pkg"]
  case exes of
    (exe:_) -> return exe
    _       -> error $  "No appropriate ghc-pkg executable in search path!\n"
                     ++ "Are you sure you have GHC " ++ cProjectVersion
                     ++ " installed?"

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
