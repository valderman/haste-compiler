module Main (main) where
import GHC
import GHC.Paths (libdir)
import HscMain
import DynFlags hiding (flags)
import TidyPgm
import CorePrep
import CoreToStg
import StgSyn (StgBinding)
import HscTypes
import GhcMonad
import System.Environment (getArgs)
import Control.Monad (when)
import CodeGen.Javascript
import Args
import ArgSpecs
import System.FilePath (addExtension)
import System.IO (openFile, hClose, IOMode (..))
import System.Process (runProcess, waitForProcess)
import System.Exit (ExitCode (..))
import System.Directory (renameFile)

main :: IO ()
main = do
  cmdargs <- getArgs
  let cmdargs' | "--opt-all" `elem` cmdargs = "-O2" : cmdargs
               | otherwise                  = cmdargs
      argRes = handleArgs defConfig argSpecs cmdargs'

  case argRes of
    Left help -> putStrLn help
    Right (cfg, ghcargs) ->
      defaultErrorHandler defaultLogAction $ runGhc (Just libdir) $ do
        let ghcargs' = "-DHASTE" : ghcargs
            args = if doTCE cfg
                     then "-DTCE" : ghcargs'
                     else ghcargs'
        dynflags <- getSessionDynFlags
        (dynflags', files, _) <- parseDynamicFlags dynflags (map noLoc args)
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
            case useGoogleClosure cfg of 
              Just clopath -> closurize clopath $ outFile cfg file
              _            -> return ()

-- | Run Google Closure on a file.
closurize :: FilePath -> FilePath -> IO ()
closurize cloPath file = do
  putStrLn $ "Running the Google Closure compiler on " ++ file ++ "..."
  let cloFile = file `addExtension` ".clo"
  cloOut <- openFile cloFile WriteMode
  build <- runProcess "java"
             ["-jar", cloPath, "--compilation_level", "ADVANCED_OPTIMIZATIONS",
              "--jscomp_off", "uselessCode", "--jscomp_off", "globalThis",
              file]
             Nothing
             Nothing
             Nothing
             (Just cloOut)
             Nothing
  res <- waitForProcess build
  hClose cloOut
  case res of
    ExitFailure n ->
      fail $ "Couldn't execute Google Closure compiler: " ++ show n
    ExitSuccess ->
      renameFile cloFile file

compile :: (GhcMonad m) => Config -> DynFlags -> ModSummary -> m ()
compile cfg dynflags modSummary = do
  case ms_hsc_src modSummary of
    HsBootFile -> liftIO $ putStrLn $ "Skipping boot " ++ myName
    _          -> do
      (pgm, name) <- prepare dynflags modSummary
      let theCode    = generate cfg name pgm
          targetpath = (targetLibPath cfg)
      liftIO $ putStrLn $ "Compiling " ++ myName ++ " into " ++ targetpath
      liftIO $ writeModule targetpath theCode
  where
    myName = moduleNameString $ moduleName $ ms_mod modSummary

prepare :: (GhcMonad m) => DynFlags -> ModSummary -> m ([StgBinding], ModuleName)
prepare dynflags theMod = do
  env <- getSession
  let name = moduleName $ ms_mod theMod
  pgm <- parseModule theMod
    >>= typecheckModule
    >>= desugarModule
    >>= liftIO . hscSimplify env . coreModule
    >>= liftIO . tidyProgram env
    >>= prepPgm . fst
    >>= liftIO . coreToStg dynflags
  return (pgm, name)
  where
    prepPgm tidy = liftIO $ do
      prepd <- corePrepPgm dynflags (cg_binds tidy) (cg_tycons tidy)
      return prepd
