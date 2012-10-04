-- | haste-pkg; wrapper for ghc-pkg.
module Main where
import Control.Monad
import System.Environment
import System.Directory
import EnvUtils

main = do
  args <- getArgs
  pkgDirExists <- doesDirectoryExist pkgDir
  when (not pkgDirExists) $ do
    runAndWait "ghc-pkg" ["init", pkgDir] Nothing
  runAndWait "ghc-pkg" args Nothing
