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
  theEnv <- getEnvironment
  h <- runProcess file (augmentArgs args) workDir (Just $ augmentEnv theEnv) Nothing Nothing Nothing
  _ <- waitForProcess h
  return ()
  where
    -- cabal can't work with GHC_PACKAGE_PATH anymore, use --package-db=foo instead.
    augmentEnv  | file == "cabal" = id
                | otherwise = (("GHC_PACKAGE_PATH", pkgDir) :)
    augmentArgs | file == "cabal" = (("--package-db=" ++ pkgDir) :)
                | otherwise = id

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
