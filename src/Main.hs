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
import Control.Applicative
import System.Environment (getArgs)
import Control.Monad (when)
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
              info = "Write the JS blob to <arg>."},
    ArgSpec { optName = "libinstall",
              updateCfg = \cfg _ -> cfg {targetLibPath = sysLibPath,
                                         performLink   = False},
              info = "Install all compiled modules into the user's jsmod "
                     ++ "library\nrather than linking them together into a JS"
                     ++ "blob."}
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
        mapM_ (compile cfg dynflags') deps
        when (performLink cfg) $ liftIO $ do
          flip mapM_ files' $ \file -> do
            putStrLn $ "Linking " ++ outFile cfg file
            link cfg file

compile :: (GhcMonad m) => Config -> DynFlags -> ModSummary -> m ()
compile cfg dynflags modSummary = do
  (pgm, name) <- prepare dynflags modSummary
  let theCode    = generate name pgm
      targetpath = (targetLibPath cfg)
  liftIO $ putStrLn $ "Compiling " ++ moduleNameString name ++ " into "
                                   ++ targetpath
  liftIO $ writeModule targetpath theCode

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
