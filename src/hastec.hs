{-# LANGUAGE CPP #-}
-- | Haste's main compiler driver.
module Main where
import Language.Haskell.GHC.Simple
import GHC
import Outputable (showPpr)

import System.Environment (getArgs)
import System.Exit
import Data.List
import System.IO.Unsafe
import System.IO
import Control.Monad
import qualified Control.Shell as Sh
import qualified Data.ByteString.UTF8 as BS

import Haste.Opts
import Haste.Args
import Haste.Config
import Haste.Environment
import Haste.Version
import Haste.Module
import Haste.CodeGen
import Haste.Linker

logStr :: Config -> String -> IO ()
logStr cfg = when (verbose cfg) . hPutStrLn stderr

main :: IO ()
main = do
    initUserPkgDB
    as <- getArgs
    let args = "-O2":concat [as,packageDBArgs,["-D__HASTE__="++show intVersion]]
    case parseHasteFlags args of
      Left act             -> act
      Right (fs, mkConfig) -> do
        let ghcconfig = mkGhcCfg fs args
        (dfs, _) <- getDynFlagsForConfig ghcconfig
        let cfg = mkLinkerCfg dfs . setShowOutputable dfs $ mkConfig def
        res <- compileFold ghcconfig (compJS cfg) [] []
        case res of
          Failure _ _         -> do
            exitFailure
          Success targets _ _ -> do
            when (performLink cfg) $ do
              mapM_ (uncurry $ linkAndMinify cfg) targets
  where
    compJS cfg targets m = do
      compJSMod cfg m
      let infile = maybe (modInterfaceFile m) id (modSourceFile m)
      if modIsTarget m
        then return $ (modPackageKey m, infile) : targets
        else return targets

    mkGhcCfg fs args = defaultConfig {
        cfgGhcFlags = fs,
        cfgGhcLibDir = Just hasteGhcLibDir,
        cfgUseTargetsFromFlags = True,
        cfgUseGhcErrorLogger = True,
        cfgUpdateDynFlags = \dfs -> dfs {
            ghcLink = NoLink,
            ghcMode = if "-c" `elem` args
                        then OneShot
                        else CompManager
          }

      }
    mkLinkerCfg dfs cfg = cfg {
        mainMod = Just (pkgKeyString $ modulePkgKey (mainModIs dfs),
                        moduleNameString $ moduleName (mainModIs dfs))
      }
    setShowOutputable dfs cfg = cfg {showOutputable = showPpr dfs}
    -- TODO: this breaks sandboxes and must be fixed
    packageDBArgs = ["-no-global-package-db",
                     "-no-user-package-db",
                     "-package-db " ++ pkgSysDir,
                     "-package-db " ++ pkgUserDir ]

-- | Compile an STG module into a JS module and write it to its appropriate
--   location according to the given config.
compJSMod :: Config -> StgModule -> IO ()
compJSMod cfg stg = do
    logStr cfg $ "Compiling " ++ myName ++ " into " ++ targetpath
    writeModule targetpath (generate cfg stg) boot
  where
    boot = modSourceIsHsBoot stg
    myName = modName stg ++ if boot then " [boot]" else ""
    targetpath = targetLibPath cfg

-- | Link a program starting from the 'mainMod' symbol of the given 'Config'.
--   Minify the result if indicated by the config.
linkAndMinify :: Config -> String -> FilePath -> IO ()
linkAndMinify cfg pkgkey infile = do
    logStr cfg $ "Linking target " ++ outfile
    link cfg (BS.fromString pkgkey) infile
    case useGoogleClosure cfg of
      Just clopath -> closurize cfg clopath outfile
      _            -> return ()
    when (outputHTML cfg) $ do
      res <- Sh.shell $ Sh.withCustomTempFile "." $ \tmp h -> do
        prog <- Sh.file outfile
        Sh.hPutStrLn h (htmlSkeleton outfile prog)
        Sh.liftIO $ hClose h
        Sh.mv tmp outfile
      case res of
        Right () -> return ()
        Left err -> error $ "Couldn't output HTML file: " ++ err
  where
    outfile = outFile cfg cfg infile

-- | Produce an HTML skeleton with an embedded JS program.
htmlSkeleton :: FilePath -> String -> String
htmlSkeleton filename prog = concat [
  "<!DOCTYPE HTML>",
  "<html><head>",
  "<title>", filename , "</title>",
  "<meta charset=\"UTF-8\">",
  "<script type=\"text/javascript\">", prog, "</script>",
  "</head><body></body></html>"]

-- | Run Google Closure on a file.
closurize :: Config -> FilePath -> FilePath -> IO ()
closurize cfg cloPath f = do
  let arguments = useGoogleClosureFlags cfg
  logStr cfg $ "Minifying " ++ f ++ "..."
  let cloFile = f `Sh.addExtension` ".clo"
  res <- Sh.shell $ do
    str <- Sh.run "java"
      (["-jar", cloPath,
        "--compilation_level", "ADVANCED_OPTIMIZATIONS",
        "--jscomp_off", "globalThis", f]
       ++ arguments) ""
    Sh.file cloFile str :: Sh.Shell ()
    Sh.mv cloFile f
  case res of
    Left e  -> fail $ "Couldn't execute Google Closure compiler: " ++ e
    Right _ -> return ()

-- | Call vanilla GHC; used for C files and the like.
callVanillaGHC :: [String] -> IO ()
callVanillaGHC args = do
    _ <- Sh.shell $ Sh.run_ ghcBinary ghcArgs ""
    return ()
  where
    Right (_, ghcArgs) = parseArgs (hasteOpts False) "" args

-- | Initialize the Haste package database, unless it already exists.
initUserPkgDB :: IO ()
initUserPkgDB = do
  _ <- Sh.shell $ do
    pkgDirExists <- Sh.isDirectory pkgUserDir
    when (not pkgDirExists) $ do
      Sh.mkdir True pkgUserLibDir
      Sh.runInteractive hastePkgBinary ["init", pkgUserDir]
  return ()

type Message = String
data BootMode = Booted | Unbooted deriving (Show, Eq)
data Compiler = Haste BootMode | GHC deriving (Show, Eq)
data RunMode = Run !Compiler | DontRun !Message deriving (Show, Eq)

rebootMsg :: Message
rebootMsg = "Haste needs to be rebooted; please run haste-boot"

-- | How should we run the compiler for this command line?
runMode :: [String] -> RunMode
runMode args
  | "--help" `elem` args                 = Run (Haste Booted)
  | "--info" `elem` args                 = DontRun ghcInfo
  | "--print-libdir" `elem` args         = DontRun hasteGhcLibDir
  | "--version" `elem` args              = DontRun $ showVersion hasteVersion
  | "--numeric-version" `elem` args      = DontRun $ showVersion ghcVersion
  | "--supported-extensions" `elem` args = DontRun exts
  | "--supported-languages" `elem` args  = DontRun exts
  | "--unbooted" `elem` args             = Run (chooseCompilerFor args Unbooted)
  | hasteNeedsReboot                     = DontRun rebootMsg
  | otherwise                            = Run (chooseCompilerFor args Booted)
  where
    exts = unlines supportedLanguagesAndExtensions
    ghcInfo = unsafePerformIO $ do
      dfs <- runGhc (Just hasteGhcLibDir) $ getSessionDynFlags
      return $ formatInfo $ compilerInfo dfs
    formatInfo = ('[' :) . tail . unlines . (++ ["]"]) . map ((',' :) . show)

-- | Parse Haste and static GHC flags, returning either an action to be taken
--   before promptly exiting, or a Haste config and a list of flags for GHC.
parseHasteFlags :: [String] -> Either (IO ()) ([String], Config -> Config)
parseHasteFlags args = do
  case runMode args of
   DontRun msg    -> Left $ putStrLn msg
   Run GHC        -> Left $ callVanillaGHC args
   Run (Haste bm) -> do
     case parseArgs (hasteOpts $ bm == Unbooted) helpHeader args of
       Left msg          -> Left $ putStrLn msg
       Right (cfg, rest) -> Right (filter (/= "-prof") rest, cfg)

-- | Use Haste or GHC for this command line?
chooseCompilerFor :: [String] -> BootMode -> Compiler
chooseCompilerFor args bm
  | all hasteOK args  = Haste bm
  | otherwise         = GHC
  where
    hasteOK f = not $ any (`isSuffixOf` f) [".c",".cmm",".cc"]
