-- | haste-inst - Haste wrapper for cabal.
module Main where
import System.Directory
import System.Process (waitForProcess, runProcess)
import System.IO.Unsafe
import System.FilePath
import System.Environment (getArgs)

hasteDir :: FilePath
hasteDir = unsafePerformIO $ getAppUserDataDirectory "haste"

cabalDir :: FilePath
cabalDir = unsafePerformIO $ getAppUserDataDirectory "cabal"

locateCompiler :: [FilePath] -> IO FilePath
locateCompiler (c:cs) = do
  exe <- findExecutable c
  case exe of
    Nothing   -> locateCompiler cs
    Just exe' -> return exe'
locateCompiler _ = do
  error "No hastec executable found; aborting!"

hasteBinary :: FilePath
hasteBinary = unsafePerformIO $ do
  locateCompiler ["hastec", cabalDir </> "bin" </> "hastec"]

runAndWait :: FilePath -> [String] -> Maybe FilePath -> IO ()
runAndWait file args workDir = do
  h <- runProcess file args workDir Nothing Nothing Nothing Nothing
  ec <- waitForProcess h
  return ()

cabal :: [String] -> IO ()
cabal args = do
  runAndWait "cabal" (hasteargs ++ args) (Just "../testlib")
  where
    hasteargs = ["--with-compiler=" ++ hasteBinary,
                 "--with-hc-pkg=ghc-pkg",
                 "--prefix=" ++ hasteDir </> "haste-install",
                 "--ghc-options=\"--libinstall\""]

main :: IO ()
main = getArgs >>= cabal
