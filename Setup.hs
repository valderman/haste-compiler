{-# LANGUAGE CPP #-}
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo hiding (libdir)
import Distribution.PackageDescription
import Control.Monad (when, forM_)
import System.Directory
import System.FilePath
import GHC.Paths
import Data.Version
import System.Info

showBootVersion :: Version -> LocalBuildInfo -> String
showBootVersion ver lbi =
    arch ++ "-" ++ myos ++ "-haste-" ++ showVersion ver ++ "-ghc-" ++ ghcver
  where
    myos
      | os == "mingw32" = "windows"
      | otherwise       = os
    ghcver =
      case compilerId $ compiler lbi of
        CompilerId GHC ver -> showVersion ver
        _                  -> error "Haste only supports building with GHC!"

portablePostBuild :: PackageDescription -> LocalBuildInfo -> IO ()
portablePostBuild pkgdesc buildinfo = do
  -- Figure out paths
  let dirname = "haste-compiler"
      exes = [ exeName exe
             | exe <- executables pkgdesc]
      builddir = buildDir buildinfo
      outdir = dirname
      versubdir = showBootVersion (pkgVersion $ package pkgdesc) buildinfo
      jsdir = outdir </> versubdir </> "js"
      datadir = dataDir $ localPkgDescr buildinfo
      jsfiles = dataFiles $ localPkgDescr buildinfo
      hastedirfile = ".hastedir" -- does Haste "own" this directory?

  dirExists <- doesDirectoryExist outdir
  isHasteDir <- doesFileExist (outdir </> hastedirfile)
  when (dirExists && not isHasteDir) $
    error $ "The output directory " ++ outdir ++ " already exists, "
            ++ "and doesn't seem to be a Haste installation."

  when (dirExists && isHasteDir) $
    removeDirectoryRecursive outdir

  -- Create directory and mark as ours
  createDirectoryIfMissing True jsdir
  createDirectoryIfMissing True (outdir </> "bin")
  writeFile (outdir </> ".hastedir") ""

  -- Copy executables
  forM_ exes $ \exe -> do
    exists <- doesFileExist $ builddir </> exe </> exe
    if exists
      then copyFile (builddir </> exe </> exe)
                    (outdir </> "bin" </> exe)
      else copyFile (builddir </> exe </> exe <.> "exe")
                    (outdir </> "bin" </> exe <.> "exe")

  -- Copy libs
  forM_ jsfiles $ \js -> do
    copyFile (datadir </> js) (jsdir </> js)

main = defaultMainWithHooks $ simpleUserHooks {
    postBuild = \_ _ pkgdesc buildinfo -> do
      if (buildinfo `has` "portable" || buildinfo `has` "portable-compiler")
        then portablePostBuild pkgdesc buildinfo
        else return ()
  }

has :: LocalBuildInfo -> String -> Bool
has lbi fn =
  case lookup (FlagName fn) (configConfigurationsFlags (configFlags lbi)) of
    Just f -> f
    _      -> False
