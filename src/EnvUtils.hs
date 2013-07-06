-- | Misc. utilities for interacting with the environment. Paths,
--   external processes, etc. go here.
module EnvUtils where
import System.Process
import System.IO.Unsafe
import System.Directory
import System.FilePath
import Data.Bits (bitSize)
import Foreign.C.Types (CInt)

-- | Host word size in bits.
hostWordSize :: Int
hostWordSize = bitSize (undefined :: CInt)

cabalDir :: FilePath
cabalDir = unsafePerformIO $ getAppUserDataDirectory "cabal"

cabalBinDir :: FilePath
cabalBinDir = cabalDir </> "bin"

hasteDir :: FilePath
hasteDir = unsafePerformIO $ getAppUserDataDirectory "haste"

libDir :: FilePath
libDir = hasteDir </> "haste-install" </> "lib"

pkgDir :: FilePath
pkgDir = hasteDir </> "haste-pkg"

runAndWait :: FilePath -> [String] -> Maybe FilePath -> IO ()
runAndWait file args workDir = do
  h <- runProcess file args workDir Nothing Nothing Nothing Nothing
  _ <- waitForProcess h
  return ()

locateBinary :: String -> [FilePath] -> IO (Either String FilePath)
locateBinary progname (c:cs) = do
  mexe <- findExecutable c
  case mexe of
    Nothing  -> locateBinary progname cs
    Just exe -> return (Right exe)
locateBinary progname _ = do
  return $ Left $ "No " ++ progname ++ " executable found; aborting!"

binaryPath :: FilePath -> FilePath
binaryPath exe = unsafePerformIO $ do
  b <- locateBinary exe [exe, cabalBinDir </> exe]
  case b of
    Left err   -> error err
    Right path -> return path

hasteBinary :: FilePath
hasteBinary = binaryPath "hastec"

hastePkgBinary :: FilePath
hastePkgBinary = binaryPath "haste-pkg"
