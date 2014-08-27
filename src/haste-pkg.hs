{-# LANGUAGE CPP #-}
-- | haste-pkg; wrapper for ghc-pkg.
module Main where
import Control.Monad
import System.Environment (getArgs)
import Haste.Environment
import Control.Shell
import System.Info (os)

main = shell $ do
  args <- liftIO getArgs
  case args of
    ["relocate", pkg] -> relocate packages pkg
    _                 -> ghcPkg packages args
  where
#if __GLASGOW_HASKELL__ >= 706
    packages = ["--global-package-db=" ++ pkgSysDir,
                "--package-db=" ++ pkgSysDir,
                "--package-db=" ++ pkgUserDir]
#else
    packages = ["--no-user-package-conf",
                "--global-conf=" ++ pkgUserDir]
#endif

ghcPkg :: [String] -> [String] -> Shell ()
ghcPkg packages args = do
  pkgDirExists <- isDirectory pkgUserDir
  when (not pkgDirExists) $ do
    mkdir True pkgUserLibDir
    runInteractive "ghc-pkg" ["init", pkgUserDir]
  pkgDirExists <- isDirectory pkgSysDir
  when (not pkgDirExists) $ do
    mkdir True pkgSysLibDir
    runInteractive "ghc-pkg" ["init", pkgSysDir]
  runInteractive "ghc-pkg" (packages ++ args)

-- | Only global packages may be marked as relocatable!
--   May break horribly for general use, only reliable for Haste base packages.
relocate :: [String] -> String -> Shell ()
relocate packages pkg = do
    pi <- run "ghc-pkg" (packages ++ ["describe", pkg]) ""
    run_ "ghc-pkg" (packages ++ ["update","-","--force","--global"]) (reloc pi)
  where
    reloc = unlines . map fixPath . lines

    fixPath s
      | isKey "library-dirs: " s       = prefix s "library-dirs" importDir
      | isKey "import-dirs: " s        = prefix s "import-dirs" importDir
      | isKey "haddock-interfaces: " s = prefix s "haddock-interfaces" importDir
      | isKey "haddock-html: " s       = prefix s "haddock-html" importDir
      | isKey "include-dirs: " s       = "include-dirs: " ++ includeDir
      | otherwise                      = s

    prefix s pfx path = pfx ++ ": " ++ path </> stripPrefix s

    stripPrefix s =
      case take 2 $ reverse $ splitPath s of
        [second, first] -> first </> second

    isKey _ "" =
      False
    isKey key str =
      and $ zipWith (==) key str

    importDir
      | os == "mingw32" = "${pkgroot}" </> "libraries"
      | otherwise       = "${pkgroot}" </> "libraries" </> "lib"
    includeDir = "${pkgroot}" </> "include"
