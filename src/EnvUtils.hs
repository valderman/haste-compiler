-- | Misc. utilities for interacting with the environment. Paths,
--   external processes, etc. go here.
module EnvUtils where
import System.Environment
import System.Process
import System.IO.Unsafe
import System.Directory
import System.FilePath

cabalDir :: FilePath
cabalDir = unsafePerformIO $ getAppUserDataDirectory "cabal"

hasteDir :: FilePath
hasteDir = unsafePerformIO $ getAppUserDataDirectory "haste"

pkgDir :: FilePath
pkgDir = hasteDir </> "haste-pkg"

runAndWait :: FilePath -> [String] -> Maybe FilePath -> IO ()
runAndWait file args workDir = do
  env <- getEnvironment
  h <- runProcess file args workDir (Just $ pkgEnv:env) Nothing Nothing Nothing
  ec <- waitForProcess h
  return ()
  where
    pkgEnv = ("GHC_PACKAGE_PATH", pkgDir)

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
