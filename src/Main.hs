{-# LANGUAGE CPP #-}
module Main (main) where
import GHC
import GHC.Paths (libdir)
import HscMain
import Outputable (showPpr)
import DynFlags hiding (flags)
import TidyPgm
import CorePrep
import CoreToStg
import StgSyn (StgBinding)
import HscTypes
import Module (moduleNameSlashes)
import GhcMonad
import System.Environment (getArgs)
import Control.Monad (when)
import Haste
import Haste.Util (showOutputable)
import Haste.Environment
import Haste.Version
import Args
import ArgSpecs
import System.FilePath (addExtension)
import System.IO
import System.Process (runProcess, waitForProcess, rawSystem)
import System.Exit (ExitCode (..), exitFailure)
import System.Directory (renameFile)
import Filesystem (getModified, isFile)
import Data.Version
import Data.List
import Data.String
import qualified Data.ByteString.Char8 as B

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
    args <- fmap (++ packageDBArgs) getArgs
    runCompiler <- preArgs args
    when (runCompiler) $ do
      if allSupported args
        then hasteMain args
        else callVanillaGHC args
  where
#if __GLASGOW_HASKELL__ >= 706
    packageDBArgs = ["-no-global-package-db",
                     "-no-user-package-db",
                     "-package-db " ++ pkgDir]
#else
    packageDBArgs = ["-no-user-package-conf",
                     "-package-conf " ++ pkgDir]
#endif
-- | Call vanilla GHC; used for boot files and the like.
callVanillaGHC :: [String] -> IO ()
callVanillaGHC args = do
  _ <- rawSystem "ghc" (filter noHasteArgs args)
  return ()
  where
    noHasteArgs x =
      x /= "--libinstall" &&
      x /= "--unbooted"

-- | Run the compiler if everything's satisfactorily booted, otherwise whine
--   and exit.
hasteMain :: [String] -> IO ()
hasteMain args
  | not needsReboot =
    compiler ("-O2" : args)
  | otherwise = do
    if "--unbooted" `elem` args
      then compiler (filter (/= "--unbooted") ("-O2" : args))
      else fail rebootMsg

-- | Determine whether all given args are handled by Haste, or if we need to
--   ship them off to vanilla GHC instead.
allSupported :: [String] -> Bool
allSupported args =
  and args'
  where
    args' = [not $ any (`isSuffixOf` a) someoneElsesProblems | a <- args]
    someoneElsesProblems = [".c", ".cmm", ".hs-boot", ".lhs-boot"]

-- | The main compiler driver.
compiler :: [String] -> IO ()
compiler cmdargs = do
  let cmdargs' | "-debug" `elem` cmdargs = "--debug":"--trace-primops":cmdargs
               | otherwise               = cmdargs
      argRes = handleArgs defConfig argSpecs cmdargs'
      usedGhcMode = if "-c" `elem` cmdargs then OneShot else CompManager

  case argRes of
    -- We got --help as an argument - display help and exit.
    Left help -> putStrLn help
    
    -- We got a config and a set of arguments for GHC; let's compile!
    Right (cfg, ghcargs) -> do
      -- Parse static flags, but ignore profiling.
      (ghcargs', _) <- parseStaticFlags [noLoc a | a <- ghcargs, a /= "-prof"]
      
      runGhc (Just libdir) $ handleSourceError (const $ liftIO exitFailure) $ do
        -- Handle dynamic GHC flags. Make sure __HASTE__ is #defined.
        let args = "-D__HASTE__" : map unLoc ghcargs'
        dynflags <- getSessionDynFlags
        (dynflags', files, _) <- parseDynamicFlags dynflags (map noLoc args)
        _ <- setSessionDynFlags dynflags' {ghcLink = NoLink,
                                           ghcMode = usedGhcMode}

        -- Prepare and compile all needed targets.
        let files' = map unLoc files
            printErrorAndDie e = printException e >> liftIO exitFailure
        deps <- handleSourceError printErrorAndDie $ do
          ts <- mapM (flip guessTarget Nothing) files'
          setTargets ts
          _ <- load LoadAllTargets
          depanal [] False
        mapM_ (compile cfg dynflags') deps
        
        -- Link everything together into a .js file.
        when (performLink cfg) $ liftIO $ do
          flip mapM_ files' $ \file -> do
            logStr $ "Linking " ++ outFile cfg file
#if __GLASGOW_HASKELL__ >= 706
            let pkgid = showPpr dynflags $ thisPackage dynflags'
#else
            let pkgid = showPpr $ thisPackage dynflags'
#endif
            link cfg pkgid file
            case useGoogleClosure cfg of 
              Just clopath -> closurize clopath $ outFile cfg file
              _            -> return ()

-- | Do everything required to get a list of STG bindings out of a module.
prepare :: (GhcMonad m) => DynFlags -> ModSummary -> m ([StgBinding], ModuleName)
prepare dynflags theMod = do
  env <- getSession
  let name = moduleName $ ms_mod theMod
  pgm <- parseModule theMod
    >>= typecheckModule
    >>= desugarModule
    >>= liftIO . hscSimplify env . coreModule
    >>= liftIO . tidyProgram env
    >>= prepPgm env . fst
    >>= liftIO . coreToStg dynflags
  return (pgm, name)
  where
    prepPgm env tidy = liftIO $ do
#if __GLASGOW_HASKELL__ >= 706
      prepd <- corePrepPgm dynflags env (cg_binds tidy) (cg_tycons tidy)
#else
      prepd <- corePrepPgm dynflags (cg_binds tidy) (cg_tycons tidy)
#endif
      return prepd


-- | Run Google Closure on a file.
closurize :: FilePath -> FilePath -> IO ()
closurize cloPath file = do
  logStr $ "Running the Google Closure compiler on " ++ file ++ "..."
  let cloFile = file `addExtension` ".clo"
  cloOut <- openFile cloFile WriteMode
  build <- runProcess "java"
             ["-jar", cloPath, "--compilation_level", "ADVANCED_OPTIMIZATIONS",
              "--jscomp_off", "globalThis", file]
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

-- | Generate a unique fingerprint for the compiler, command line arguments,
--   etc.
genFingerprint :: String -> FilePath -> [String] -> Fingerprint
genFingerprint modname targetpath args =
  {- md5sum $ B.pack $ -} show [
      modname,
      targetpath,
      show bootVersion,
      show args
    ]

-- | Compile a module into a .jsmod intermediate file.
compile :: (GhcMonad m) => Config -> DynFlags -> ModSummary -> m ()
compile cfg dynflags modSummary = do
    fp <- liftIO $ fmap (genFingerprint myName targetpath) getArgs
    should_recompile <- liftIO $ shouldRecompile fp modSummary targetpath
    when should_recompile $ do
      case ms_hsc_src modSummary of
        HsBootFile -> liftIO $ logStr $ "Skipping boot " ++ myName
        _          -> do
          (pgm, name) <- prepare dynflags modSummary
#if __GLASGOW_HASKELL__ >= 706
          let pkgid = showPpr dynflags $ modulePackageId $ ms_mod modSummary
#else
          let pkgid = showPpr $ modulePackageId $ ms_mod modSummary
#endif
              theCode = generate cfg fp pkgid name pgm
          liftIO $ logStr $ "Compiling " ++ myName ++ " into " ++ targetpath
          liftIO $ writeModule targetpath theCode
  where
    myName = moduleNameString $ moduleName $ ms_mod modSummary
    targetpath = targetLibPath cfg

shouldRecompile :: Fingerprint -> ModSummary -> FilePath -> IO Bool
shouldRecompile _ _ _ = return True
{-
shouldRecompile fp ms path = do
    exists <- isFile fpPath
    if exists
      then do
        fp' <- readModuleFingerprint path pkgid modname
        jsmtime <- getModified fpPath
        return $ ms_hs_date ms > jsmtime || fp /= fp'
      else do
        return True
  where
    modname = moduleNameString $ ms_mod_name ms
    file    = moduleNameSlashes (ms_mod_name ms) ++ ".jsmod"
    path'   = path ++ "/" ++ pkgid ++ "/" ++ file
    pkgid   = showOutputable $ modulePackageId $ ms_mod ms
    fpPath  = fromString path'
-}
