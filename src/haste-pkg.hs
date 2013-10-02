{-# LANGUAGE CPP #-}
-- | haste-pkg; wrapper for ghc-pkg.
module Main where
import Control.Monad
import System.Environment
import System.Directory
import Haste.Environment

main = do
  args <- getArgs
  pkgDirExists <- doesDirectoryExist pkgDir
  when (not pkgDirExists) $ do
    createDirectoryIfMissing True pkgLibDir
    runAndWait "ghc-pkg" ["init", pkgDir] Nothing
  runAndWait "ghc-pkg" (packages ++ map userToGlobal args) Nothing
  where
#if __GLASGOW_HASKELL__ >= 706
    packages = ["--no-user-package-db",
                "--global-package-db=" ++ pkgDir]
#else
    packages = ["--no-user-package-conf",
                "--global-conf=" ++ pkgDir]
#endif
    userToGlobal "--user" = "--global"
    userToGlobal str      = str
