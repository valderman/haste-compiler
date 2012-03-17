module Main (main) where
import GHC
import GHC.Paths (libdir)
import HscMain
import DynFlags hiding (flags)
import TidyPgm
import CorePrep
import CoreSyn
import HscTypes
import GhcMonad
import System.Directory
import System.FilePath (combine)
import Control.Applicative
import System.Environment (getArgs)
import CodeGen.Javascript
import Args

argSpecs :: [ArgSpec Config]
argSpecs = [
    ArgSpec { optName = "debug",
              updateCfg = \cfg _ -> cfg {rtsLibs = debugRtsLib : rtsLibs cfg,
                                         ppOpts  = pretty},
              info = "Link in the debug library and output readable code."},
    ArgSpec { optName = "start=onload",
              updateCfg = \cfg _ -> cfg {appStart = startOnDocumentLoad},
              info = "Start program on document load instead of immediately."},
    ArgSpec { optName = "out=",
              updateCfg = \cfg outfile -> cfg {outFile = const $ head outfile},
              info = "Write the JS blob to <arg>."}
  ]

main :: IO ()
main = do
  argRes <- handleArgs defConfig argSpecs <$> getArgs
  case argRes of
    Left help -> putStrLn help
    Right (cfg, ghcargs) ->
      defaultErrorHandler defaultLogAction $ runGhc (Just libdir) $ do
        dynflags <- getSessionDynFlags
        (dynflags', files, _) <- parseDynamicFlags dynflags (map noLoc ghcargs)
        _ <- setSessionDynFlags dynflags' {ghcLink = NoLink}
        let files' = map unLoc files

        ts <- mapM (flip guessTarget Nothing) files'
        setTargets ts
        _ <- load LoadAllTargets
        deps <- depanal [] False
        mapM_ (compile dynflags') deps
        liftIO $ mapM_ (link cfg) files'

compile :: (GhcMonad m) => DynFlags -> ModSummary -> m ()
compile dynflags modSummary = do
  (pgm, name) <- prepare dynflags modSummary
  targetdir <- getTargetDir []
  let theCode = generate name pgm
  liftIO $ writeModule targetdir theCode

prepare :: (GhcMonad m) => DynFlags -> ModSummary -> m (CoreProgram, ModuleName)
prepare dynflags theMod = do
  env <- getSession
  let name = moduleName $ ms_mod theMod
  pgm <- parseModule theMod
    >>= typecheckModule
    >>= desugarModule
    >>= liftIO . hscSimplify env . coreModule
    >>= liftIO . tidyProgram env
    >>= prepPgm . fst
  return (pgm, name)
  where
    prepPgm tidy = liftIO $ do
      prepd <- corePrepPgm dynflags (cg_binds tidy) (cg_tycons tidy)
      return prepd

getTargetDir :: GhcMonad m => [String] -> m FilePath
getTargetDir xs
  | any (== "libinstall") xs =
    liftIO $ append "lib" <$> getAppUserDataDirectory "jsplug"
  | otherwise =
    return "."

append :: FilePath -> FilePath -> FilePath
append = flip combine