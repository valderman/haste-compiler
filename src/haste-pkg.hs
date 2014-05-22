{-# LANGUAGE CPP #-}
-- | haste-pkg; wrapper for ghc-pkg.
module Main where
import Control.Monad
import System.Environment (getArgs)
import Haste.Environment
import Control.Shell

main = shell $ do
  args <- liftIO getArgs
  pkgDirExists <- isDirectory pkgDir
  when (not pkgDirExists) $ do
    mkdir True pkgLibDir
    runInteractive "ghc-pkg" ["init", pkgDir]
  runInteractive "ghc-pkg" (packages ++ map userToGlobal args)
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
