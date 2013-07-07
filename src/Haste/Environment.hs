-- | Paths, host bitness and other environmental information about Haste.
module Haste.Environment where
import System.Process
import System.IO.Unsafe
import System.Directory
import System.FilePath
import Data.Bits (bitSize)
import Foreign.C.Types (CIntPtr)

-- | Host word size in bits.
hostWordSize :: Int
hostWordSize = bitSize (undefined :: CIntPtr)

-- | Directory where cabal resides. Bundled JS files end up here.
cabalDir :: FilePath
cabalDir = unsafePerformIO $ getAppUserDataDirectory "cabal"

-- | Cabal dir for binaries.
cabalBinDir :: FilePath
cabalBinDir = cabalDir </> "bin"

-- | Directory housing hi files, jsmods and other stuff Haste spits out.
hasteDir :: FilePath
hasteDir = unsafePerformIO $ getAppUserDataDirectory "haste"

jsmodDir :: FilePath
jsmodDir = hasteDir </> "lib"

-- | Directory containing library information. 
pkgLibDir :: FilePath
pkgLibDir = hasteInstDir </> "lib"

-- | Base directory for haste-inst.
hasteInstDir :: FilePath
hasteInstDir = hasteDir </> "haste-install"

-- | Directory housing package information.
pkgDir :: FilePath
pkgDir = hasteDir </> "haste-pkg"

-- | Run a process and wait for its completion.
runAndWait :: FilePath -> [String] -> Maybe FilePath -> IO ()
runAndWait file args workDir = do
  h <- runProcess file args workDir Nothing Nothing Nothing Nothing
  _ <- waitForProcess h
  return ()

-- | Find an executable.
locateBinary :: String -> [FilePath] -> IO (Either String FilePath)
locateBinary progname (c:cs) = do
  mexe <- findExecutable c
  case mexe of
    Nothing  -> locateBinary progname cs
    Just exe -> return (Right exe)
locateBinary progname _ = do
  return $ Left $ "No " ++ progname ++ " executable found; aborting!"

-- | Find a binary that's probably in Cabal's bin directory.
binaryPath :: FilePath -> FilePath
binaryPath exe = unsafePerformIO $ do
  b <- locateBinary exe [exe, cabalBinDir </> exe]
  case b of
    Left err   -> error err
    Right path -> return path

-- | The main Haste compiler binary.
hasteBinary :: FilePath
hasteBinary = binaryPath "hastec"

-- | Binary for haste-pkg.
hastePkgBinary :: FilePath
hastePkgBinary = binaryPath "haste-pkg"

-- | JAR for Closure compiler.
closureCompiler :: FilePath
closureCompiler = hasteDir </> "compiler.jar"
