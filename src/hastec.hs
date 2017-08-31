{-# LANGUAGE CPP #-}
-- | Haste's main compiler driver.
module Main where
import Language.Haskell.GHC.Simple as GHC
import Language.Haskell.GHC.Simple.PrimIface
import Packages
import GHC
import Outputable (showPpr)
import Platform

import System.Environment (getArgs, lookupEnv, setEnv)
import System.Exit
import Data.List
import System.IO.Unsafe
import System.IO
import System.FilePath
import System.Directory (copyFile)
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
import Haste.AST as AST (Module)
import Haste.Linker
import Haste.JSLib

logStr :: Config -> String -> IO ()
logStr cfg = when (verbose cfg) . hPutStrLn stderr

main :: IO ()
main = do
    initUserPkgDB
    as <- getArgs
    lookupEnv "HASTE_PACKAGE_PATH" >>= setEnv "GHC_PACKAGE_PATH" . maybe "" id
    booting <- maybe False (const True) `fmap` lookupEnv "HASTE_BOOTING"
    let args = "-O2":concat [pkgs booting, as, ["-D__HASTE__="++show intVersion]]
        prof = "-prof" `elem` args
    case parseHasteFlags booting args as of
      Left act             -> act
      Right (fs, mkConfig) -> do
        let hastecfg = mkConfig Haste.Config.defaultConfig
            ghccfg = mkGhcCfg hastecfg fs args
        (dfs, _) <- getDynFlagsForConfig ghccfg
        extralibdirs <- getExtraLibDirs dfs
        let cfg = mkLinkerCfg dfs extralibdirs
                . setShowOutputable dfs
                $ hastecfg
        res <- compileFold ghccfg (compJSMod cfg) finalize ([], []) []
        case res of
          Failure _ _         -> do
            exitFailure
          Success (targets, mods) _ _ -> do
            when (performLink cfg) $ do
              if linkJSLib cfg
                then buildJSLib prof cfg (fst $ head targets) mods
                else mapM_ (uncurry $ linkAndMinify cfg) targets
  where
    getExtraLibDirs = fmap (concatMap libraryDirs) . readPackageConfigs

    pkgs booting =
      ["-no-global-package-db",
       "-no-user-package-db",
       "-package-db=" ++ pkgSysDir] ++
      if booting && portableHaste
        then []
        else ["-package-db=" ++ pkgUserDir]

    buildJSLib profiling cfg pkgkey mods = do
      let targetpath  = targetLibPath cfg
          mods'       = [moduleFilePath targetpath mn False | mn <- mods]
          libfile     = maybe (pkgkey <.> "jsmod") id (linkJSLibFile cfg)
          profLibfile = reverse (drop 6 (reverse libfile)) ++ "_p.jslib"

      createJSLib libfile (BS.fromString pkgkey) mods'
      when profiling . void $ do
        copyFile libfile (profLibfile)

    finalize (targets, mods) m = do
      let meta = modMetadata m
          infile = maybe (mmInterfaceFile meta) id (mmSourceFile meta)
          modpair = (mmPackageKey meta, infile)
      if modIsTarget m
        then return (modpair : targets, mmName meta : mods)
        else return (targets, mmName meta : mods)

    mkGhcCfg cfg fs args = disableCodeGen $ GHC.defaultConfig {
        cfgGhcFlags = fs,
        cfgGhcLibDir = Just hasteGhcLibDir,
        cfgUseTargetsFromFlags = True,
        cfgUseGhcErrorLogger = True,
        cfgCacheDirectory = Just $ targetLibPath cfg,
        cfgCacheFileExt = "jsmod",
        cfgUpdateDynFlags = \dfs -> dfs {
            ghcMode = if "-c" `elem` args
                        then OneShot
                        else CompManager,
            settings = (settings dfs) {
                sTargetPlatform = (sTargetPlatform $ settings dfs) {
                    platformArch             = ArchX86,
                    platformWordSize         = 4
                  },
                sPlatformConstants = (sPlatformConstants $ settings dfs) {
                    pc_WORD_SIZE       = 4,
                    pc_CINT_SIZE       = 4,
                    pc_CLONG_SIZE      = 4,
                    pc_CLONG_LONG_SIZE = 8,
                    pc_DOUBLE_SIZE     = 8,
                    pc_WORDS_BIGENDIAN = False
                  }
             }
          }
        , cfgCustomPrimIface = Just (primOpInfo, primOpStrictness)
      }
    mkLinkerCfg dfs extralibdirs cfg = cfg {
        mainMod = Just (pkgKeyString $ modulePkgKey (mainModIs dfs),
                        moduleNameString $ moduleName (mainModIs dfs)),
        libPaths = extralibdirs ++ libPaths cfg
      }
    setShowOutputable dfs cfg = cfg {showOutputable = showPpr dfs}

-- | Primop info for custom GHC.Prim interface.
primOpInfo :: PrimOp -> PrimOpInfo
#include "primop-info-710.hs"
primOpInfo _ = error "Nonexistent primop!"

-- | Strictness info for custom GHC.Prim interface.
primOpStrictness :: PrimOp -> Arity -> StrictSig
#include "primop-stricts-710.hs"

-- | Compile an STG module into a JS module and write it to its appropriate
--   location according to the given config.
compJSMod :: Config -> ModMetadata -> [StgBinding] -> IO AST.Module
compJSMod cfg meta stg = do
    logStr cfg $ "Compiling " ++ myName ++ " into " ++ targetpath
    return $ generate cfg meta stg
  where
    boot = mmSourceIsHsBoot meta
    myName = mmName meta ++ if boot then " [boot]" else ""
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
      res <- Sh.shell $ Sh.withCustomTempFile Sh.TextMode "." $ \tmp h -> do
        prog <- Sh.input outfile
        Sh.hPutStrLn h (htmlSkeleton outfile prog)
        Sh.liftIO $ hClose h
        Sh.mv tmp outfile
      case res of
        Right () -> return ()
        Left err -> error $ "Couldn't output HTML file: " ++ Sh.exitString err
  where
    outfile = outFile cfg cfg infile

-- | Produce an HTML skeleton with an embedded JS program.
htmlSkeleton :: FilePath -> String -> String
htmlSkeleton filename prog = concat [
  "<!DOCTYPE HTML>",
  "<html><head>",
  "<title>", filename , "</title>",
  "<meta charset=\"UTF-8\">",
  "<meta http-equiv=\"x-ua-compatible\" content=\"ie=edge, chrome=1\">",
  "<script type=\"text/javascript\">", prog, "</script>",
  "</head><body></body></html>"]

-- | Run Google Closure on a file.
closurize :: Config -> FilePath -> FilePath -> IO ()
closurize cfg cloPath f = do
  let arguments = useGoogleClosureFlags cfg
  logStr cfg $ "Minifying " ++ f ++ "..."
  let cloFile = f `Sh.addExtension` ".clo"
  res <- Sh.shell $ do
    Sh.run "java" (cloArgs arguments cloFile)
    Sh.mv cloFile f
  case res of
    Left e  -> fail $ "Couldn't execute Google Closure compiler: " ++
                      Sh.exitString e
    Right _ -> return ()
  where
    cloArgs args cloFile =
      [ "-jar"
      , cloPath
      , "--compilation_level", "ADVANCED_OPTIMIZATIONS"
      , "--jscomp_off", "globalThis"
      , "--js_output_file", cloFile
      , f
      ] ++ args


-- | Call vanilla GHC; used for C files and the like.
callVanillaGHC :: [String] -> IO ()
callVanillaGHC args = do
    booting <- maybe False (const True) `fmap` lookupEnv "HASTE_BOOTING"
    Sh.shell_ $ Sh.run ghcBinary (pkgargs booting ++ ghcArgs)
  where
    Right (_, ghcArgs, _) = parseArgs hasteOpts "" args
    pkgargs booting =
      ["-no-global-package-db",
       "-no-user-package-db",
       "-package-db=" ++ pkgSysDir] ++
      if booting && portableHaste
        then []
        else ["-package-db=" ++ pkgUserDir]

-- | Initialize the Haste package database, unless it already exists.
initUserPkgDB :: IO ()
initUserPkgDB = do
  Sh.shell_ $ do
    pkgDirExists <- Sh.isDirectory pkgUserDir
    when (not pkgDirExists) $ do
      Sh.mkdir True pkgUserLibDir
      Sh.run hastePkgBinary ["init", pkgUserDir]

type Message = String
data Compiler = Haste | GHC | InstallJSExe | BuildRunner deriving (Show, Eq)
data RunMode = Run !Compiler | DontRun !Message deriving (Show, Eq)

rebootMsg :: Message
rebootMsg = "Haste needs to be rebooted; please run haste-boot"

-- | How should we run the compiler for this command line?
runMode :: Bool -> [String] -> RunMode
runMode booting as
  | "--help" `elem` as                    = Run Haste
  | "--install-executable" `elem` as      = Run InstallJSExe
  | "--build-runner" `elem` as            = Run BuildRunner
  | "--info" `elem` as                    = DontRun ghcInfo
  | "--print-libdir" `elem` as            = DontRun hasteGhcLibDir
  | "--print-global-package-db" `elem` as = DontRun pkgSysDir
  | "--version" `elem` as                 = DontRun $ showVersion hasteVersion
  | "--numeric-ghc-version" `elem` as     = DontRun $ showVersion ghcVersion
  | "--supported-extensions" `elem` as    = DontRun exts
  | "--supported-languages" `elem` as     = DontRun exts
  | hasteNeedsReboot && not booting       = DontRun rebootMsg
  | otherwise                             = Run (chooseCompilerFor as)
  where
    exts = unlines supportedLanguagesAndExtensions
    ghcInfo = unsafePerformIO $ do
      dfs <- runGhc (Just hasteGhcLibDir) $ getSessionDynFlags
      return $ formatInfo $ compilerInfo dfs
    formatInfo = ('[' :) . tail . unlines . (++ ["]"]) . map ((',' :) . show)

-- | Parse Haste and static GHC flags, returning either an action to be taken
--   before promptly exiting, or a Haste config and a list of flags for GHC.
parseHasteFlags :: Bool -> [String] -> [String]
                -> Either (IO ()) ([String], Config->Config)
parseHasteFlags booting args rawargs = do
  case runMode booting args of
    _ | null rawargs -> Left $ putStrLn noInputFiles
    DontRun msg -> Left $ putStrLn msg
    Run GHC -> Left $ callVanillaGHC args
    Run Haste -> do
      case parseArgs hasteOpts helpHeader args of
        Left msg              -> Left $ putStrLn msg
        Right (cfg, rest, []) -> Right (filter (/= "-prof") rest, cfg)
        Right (_, _,      es) -> Left (mapM_ putStr es >> exitFailure)
    Run InstallJSExe -> do
      Left $ installJSExe (jsexeIn args) (jsexeOut args)
    Run BuildRunner -> do
      Left $ callVanillaGHC (rawargs ++ ["-package-id", "Cabal-1.23.0.0-f701f4ea98ec7bed5883c4df743045e6"])
  where
     noInputFiles = init $ unlines
       [ "hastec: no input files"
       , "try the `--help' option for usage information"
       ]
     jsexeIn ("--install-executable":exe:_) = exe
     jsexeIn (_:xs)                         = jsexeIn xs
     jsexeIn _                              = error "No executable to install!"

     jsexeOut ("-o":out:_) = out
     jsexeOut (_:xs)       = jsexeOut xs
     jsexeOut _            = error "No executable install target!"

-- | Install executable as an HTML file.
installJSExe :: FilePath -> FilePath -> IO ()
installJSExe inf outf = do
  js <- readFile inf
  if (take 2 js == "<!")
    then writeFile (replaceExtension outf "html") js
    else writeFile (replaceExtension outf "html") (htmlSkeleton outf js)

-- | Use Haste or GHC for this command line?
chooseCompilerFor :: [String] -> Compiler
chooseCompilerFor args
  | all hasteOK args  = Haste
  | otherwise         = GHC
  where
    hasteOK f = not $ any (`isSuffixOf` f) [".c",".cmm",".cc",".dyn_o"]
