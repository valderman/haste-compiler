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
    runInteractive ghcPkgBinary ["init", pkgUserDir]
  pkgDirExists <- isDirectory pkgSysDir
  when (not pkgDirExists) $ do
    mkdir True pkgSysLibDir
    runInteractive ghcPkgBinary ["init", pkgSysDir]
  runInteractive ghcPkgBinary (packages ++ args)

-- | Only global packages may be marked as relocatable!
--   May break horribly for general use, only reliable for Haste base packages.
relocate :: [String] -> String -> Shell ()
relocate packages pkg = do
    pi <- run ghcPkgBinary (packages ++ ["describe", pkg]) ""
    run_ ghcPkgBinary (packages++["update","-","--force","--global"]) (reloc pi)
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

    stripPrefix s
      | os == "darwin" =
        case take 3 $ reverse $ splitPath s of
          [third, second, first] -> first </> second </> third
      | otherwise =
        case take 2 $ reverse $ splitPath s of
          [second, first] -> first </> second

    isKey _ "" =
      False
    isKey key str =
      and $ zipWith (==) key str

    importDir
      | os == "linux"  = "${pkgroot}" </> "libraries" </> "lib"
      | otherwise      = "${pkgroot}" </> "libraries"
    includeDir = "${pkgroot}" </> "include"
