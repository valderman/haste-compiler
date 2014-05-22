{-# LANGUAGE CPP #-}
-- | Paths, host bitness and other environmental information about Haste.
module Haste.Environment (hasteDir, jsmodDir, hasteInstDir, pkgDir, pkgLibDir,
                          jsDir, hostWordSize, hasteBinary,
                          hastePkgBinary, hasteInstHisBinary, hasteInstBinary,
                          hasteCopyPkgBinary, closureCompiler) where
import System.IO.Unsafe
import Data.Bits (bitSize)
import Foreign.C.Types (CIntPtr)
import System.Environment.Executable
import System.Exit
import Control.Shell
import Paths_haste_compiler

-- | The directory where the currently residing binary lives.
currentBinDir :: FilePath
currentBinDir = dropFileName . unsafePerformIO $ getExecutablePath

#if defined(PORTABLE) || defined(PORTABLE_COMPILER)
hasteBinDir :: FilePath
hasteBinDir = currentBinDir

jsDir :: FilePath
jsDir = hasteBinDir </> "js"
#else
hasteBinDir :: FilePath
hasteBinDir = unsafePerformIO $ getBinDir

jsDir :: FilePath
jsDir = unsafePerformIO $ getDataDir
#endif

#if defined(PORTABLE)
hasteDir :: FilePath
hasteDir = hasteBinDir
#else
hasteDir :: FilePath
Right hasteDir = unsafePerformIO . shell $ withAppDirectory "haste" return
#endif

jsmodDir :: FilePath
jsmodDir = hasteDir </> "jsmods"

-- | Base directory for haste-inst.
hasteInstDir :: FilePath
hasteInstDir = hasteDir </> "libraries"

-- | Directory housing package information.
pkgDir :: FilePath
pkgDir = hasteDir </> "packages"

-- | Host word size in bits.
hostWordSize :: Int
hostWordSize = bitSize (undefined :: CIntPtr)

-- | Directory containing library information. 
pkgLibDir :: FilePath
pkgLibDir = hasteInstDir </> "lib"

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
closureCompiler = hasteDir </> "compiler.jar"
