-- | Misc. utilities for interacting with the environment. Paths,
--   external processes, etc. go here.
module EnvUtils where
import System.Process
import System.IO.Unsafe
import System.Directory
import System.FilePath

cabalDir :: FilePath
cabalDir = unsafePerformIO $ getAppUserDataDirectory "cabal"

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

locateCompiler :: [FilePath] -> IO (Maybe FilePath)
locateCompiler (c:cs) = do
  exe <- findExecutable c
  case exe of
    Nothing -> locateCompiler cs
    _       -> return exe
locateCompiler _ = do
  putStrLn "No hastec executable found; aborting!"
  return Nothing

hasteBinary :: FilePath
Just hasteBinary = unsafePerformIO $ do
  locateCompiler ["hastec", cabalDir </> "bin" </> "hastec"]
