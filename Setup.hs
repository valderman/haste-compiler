{-# LANGUAGE CPP #-}
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Control.Monad (when, forM_)
import System.Directory
import System.FilePath

main = defaultMainWithHooks $ simpleUserHooks {
    postBuild = \args buildflags pkgdesc buildinfo -> do
       when (buildinfo `has` "portable" ||
             buildinfo `has` "portable-compiler") $ do
         -- Figure out paths
         let dirname = "haste-compiler"
             exes = [ exeName exe ++ fromPathTemplate (progSuffix buildinfo)
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
             
         createDirectoryIfMissing True (outdir </> "js")
         
         -- Copy executables
         forM_ exes $ \exe -> do
           copyFile (builddir </> exe </> exe) (outdir </> exe)
         
         -- Copy libs
         forM_ jsfiles $ \js -> do
           copyFile (datadir </> js) (outdir </> "js" </> js)
         
         -- Mark the directory as ours
         writeFile (outdir </> ".hastedir") ""
  }

has :: LocalBuildInfo -> String -> Bool
has lbi fn =
  case lookup (FlagName fn) (configConfigurationsFlags (configFlags lbi)) of
    Just f -> f
    _      -> False