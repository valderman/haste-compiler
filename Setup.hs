{-# LANGUAGE CPP #-}
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo hiding (libdir)
import Distribution.PackageDescription
import Control.Monad (when, forM_)
import System.Directory
import System.FilePath
import GHC.Paths

main = defaultMainWithHooks $ simpleUserHooks {
    postBuild = \args buildflags pkgdesc buildinfo -> do
       when (buildinfo `has` "portable" ||
             buildinfo `has` "portable-compiler") $ do
         -- Figure out paths
         let dirname = "haste-compiler"
             exes = [ exeName exe
                    | exe <- executables pkgdesc]
             builddir = buildDir buildinfo
             outdir = dirname
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
         createDirectoryIfMissing True (outdir </> "js")
         createDirectoryIfMissing True (outdir </> "bin")
         writeFile (outdir </> ".hastedir") ""

         -- Copy executables
         forM_ exes $ \exe -> do
           exists <- doesFileExist $ builddir </> exe </> exe
           if exists
             then copyFile (builddir </> exe </> exe) (outdir </> "bin" </> exe)
             else copyFile (builddir </> exe </> exe <.> "exe")
                           (outdir </> "bin" </> exe <.> "exe")
         
         -- Copy libs
         forM_ jsfiles $ \js -> do
           copyFile (datadir </> js) (outdir </> "js" </> js)
  }

has :: LocalBuildInfo -> String -> Bool
has lbi fn =
  case lookup (FlagName fn) (configConfigurationsFlags (configFlags lbi)) of
    Just f -> f
    _      -> False
