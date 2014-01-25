{-# LANGUAGE CPP #-}
-- | Paths, host bitness and other environmental information about Haste.
module Haste.Environment (hasteDir, jsmodDir, hasteInstDir, pkgDir, pkgLibDir,
                          jsDir, hostWordSize, runAndWait, hasteBinary,
                          hastePkgBinary, hasteInstHisBinary, hasteInstBinary,
                          hasteCopyPkgBinary, closureCompiler) where
import System.Process
import System.IO.Unsafe
import System.FilePath
import Data.Bits (bitSize)
import Foreign.C.Types (CIntPtr)
import System.Environment.Executable
import System.Directory
import System.Exit
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
hasteDir = unsafePerformIO $ getAppUserDataDirectory "haste"
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

-- | Run a process and wait for its completion. Terminate with an error code
--   if the process did not exit cleanly.
runAndWait :: FilePath -> [String] -> Maybe FilePath -> IO ()
runAndWait file args workDir = do
  h <- runProcess file args workDir Nothing Nothing Nothing Nothing
  ec <- waitForProcess h
  case ec of
    ExitFailure _ -> exitFailure
    _             -> return ()

{-
-- | Find an executable.
locateBinary :: String -> [FilePath] -> IO (Either String FilePath)
locateBinary progname (c:cs) = do
  mexe <- findExecutable c
  case mexe of
    Nothing  -> locateBinary progname cs
    Just exe -> return (Right exe)
locateBinary progname _ = do
  return $ Left $ "No " ++ progname ++ " executable found; aborting!"

-- | Find a binary.
binaryPath :: FilePath -> FilePath
binaryPath exe = unsafePerformIO $ do
  b <- locateBinary exe [exe, currentBinDir </> exe, cabalBinDir </> exe]
  case b of
    Left err   -> error err
    Right path -> return path
-}

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
