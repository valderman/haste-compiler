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
import System.IO
import System.Process (runProcess, waitForProcess, rawSystem)
import System.Exit (ExitCode (..))
import System.Directory (renameFile)
import Version
import Data.Version
import Data.List
import EnvUtils
import System.Posix.Env (setEnv)

logStr :: String -> IO ()
logStr = hPutStrLn stderr

rebootMsg :: String
rebootMsg = "Haste needs to be rebooted; please run haste-boot"

printInfo :: IO ()
printInfo = do
  ghc <- runGhc (Just libdir) getSessionDynFlags
  putStrLn $ formatInfo $ compilerInfo ghc
  where
    formatInfo = ('[' :) . tail . unlines . (++ ["]"]) . map ((',' :) . show)

-- | Check for arguments concerning version info and the like, and act on them.
--   Return True if the compiler should run afterwards.
preArgs :: [String] -> IO Bool
preArgs args
  | "--numeric-version" `elem` args =
    putStrLn ghcVersion >> return False
  | "--info" `elem` args =
    printInfo >> return False
  | "--version" `elem` args =
    putStrLn (showVersion hasteVersion) >> return False
  | "--supported-extensions" `elem` args =
    (putStrLn $ unlines $ supportedLanguagesAndExtensions) >> return False
  | "--supported-languages" `elem` args =
    (putStrLn $ unlines $ supportedLanguagesAndExtensions) >> return False
  | otherwise =
    return True

main :: IO ()
main = do
  setEnv "GHC_PACKAGE_PATH" pkgDir True
  args <- getArgs
  runCompiler <- preArgs args
  when (runCompiler) $ do
    if allSupported args
      then hasteMain args
      else callVanillaGHC args

callVanillaGHC :: [String] -> IO ()
callVanillaGHC args = do
  _ <- rawSystem "ghc" (filter (/= "--libinstall") args)
  return ()

hasteMain :: [String] -> IO ()
hasteMain args
  | needsReboot == Dont =
    compiler args
  | otherwise = do
    if "--unbooted" `elem` args
      then compiler (filter (/= "--unbooted") args)
      else fail rebootMsg

allSupported :: [String] -> Bool
allSupported args =
  and args'
  where
    args' = [not $ any (`isSuffixOf` a) [".c", ".cmm"] | a <- args]

compiler :: [String] -> IO ()
compiler cmdargs = do
  let cmdargs' | "--opt-all" `elem` cmdargs = "-O2" : cmdargs
               | "--opt-all-unsafe" `elem` cmdargs = "-O2" : cmdargs
               | otherwise                  = cmdargs
      argRes = handleArgs defConfig argSpecs cmdargs'
      usedGhcMode = if "-c" `elem` cmdargs then OneShot else CompManager

  case argRes of
    Left help -> putStrLn help
    Right (cfg, ghcargs) ->
      defaultErrorHandler defaultLogAction $ runGhc (Just libdir) $ do
        let ghcargs' = "-D__HASTE__" : ghcargs
            args = if doTCE cfg
                     then "-D__HASTE_TCE__" : ghcargs'
                     else ghcargs'
        dynflags <- getSessionDynFlags
        (dynflags', files, _) <- parseDynamicFlags dynflags (map noLoc args)
        _ <- setSessionDynFlags dynflags' {ghcLink = NoLink,
                                           ghcMode = usedGhcMode}
        let files' = map unLoc files

        ts <- mapM (flip guessTarget Nothing) files'
        setTargets ts
        _ <- load LoadAllTargets
        deps <- depanal [] False
        mapM_ (compile cfg dynflags') deps
        when (performLink cfg) $ liftIO $ do
          flip mapM_ files' $ \file -> do
            logStr $ "Linking " ++ outFile cfg file
            link cfg file
            case useGoogleClosure cfg of 
              Just clopath -> closurize clopath $ outFile cfg file
              _            -> return ()

-- | Run Google Closure on a file.
closurize :: FilePath -> FilePath -> IO ()
closurize cloPath file = do
  logStr $ "Running the Google Closure compiler on " ++ file ++ "..."
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
    HsBootFile -> liftIO $ logStr $ "Skipping boot " ++ myName
    _          -> do
      (pgm, name) <- prepare dynflags modSummary
      let theCode    = generate cfg name pgm
          targetpath = (targetLibPath cfg)
      liftIO $ logStr $ "Compiling " ++ myName ++ " into " ++ targetpath
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
