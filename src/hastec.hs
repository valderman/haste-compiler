{-# LANGUAGE CPP #-}
-- | Haste's main compiler driver.
module Main where
import Language.Haskell.GHC.Simple
#if __GLASGOW_HASKELL__ >= 710
import Language.Haskell.GHC.Simple.PrimIface
import Packages
import PackageConfig
#endif
import GHC
import Outputable (showPpr)

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
        let ghcconfig = mkGhcCfg fs args
        (dfs, _) <- getDynFlagsForConfig ghcconfig
        extralibdirs <- getExtraLibDirs dfs
        putStrLn $ "EXTRALIBDIRS: " ++ show extralibdirs
        let cfg = mkLinkerCfg dfs extralibdirs
                . setShowOutputable dfs
                $ mkConfig def
        res <- compileFold ghcconfig (compJS cfg) ([], []) []
        case res of
          Failure _ _         -> do
            exitFailure
          Success (targets, mods) _ _ -> do
            when (performLink cfg) $ do
              if linkJSLib cfg
                then buildJSLib prof cfg (fst $ head targets) mods
                else mapM_ (uncurry $ linkAndMinify cfg) targets
  where
#if __GLASGOW_HASKELL__ >= 710
    getExtraLibDirs = fmap (concatMap libraryDirs) . readPackageConfigs
#else
    getExtraLibDirs = const (return [])
#endif

    dotToSlash '.' = '/'
    dotToSlash c   = c

    pkgs booting =
      ["-no-global-package-db",
       "-no-user-package-db",
       "-package-db=" ++ pkgSysDir] ++
      if booting && portableHaste
        then []
        else ["-package-db=" ++ pkgUserDir]

    buildJSLib profiling cfg pkgkey mods = do
      let modpath     = targetLibPath cfg ++ "/" ++ pkgkey
          mods'       = [ modpath ++ "/" ++ map dotToSlash mn ++ ".jsmod"
                        | mn <- mods]
          libfile     = maybe (pkgkey <.> "jsmod") id (linkJSLibFile cfg)
          profLibfile = reverse (drop 6 (reverse libfile)) ++ "_p.jslib"

      createJSLib libfile (BS.fromString pkgkey) mods'
      when profiling . void $ do
        copyFile libfile (profLibfile)

    compJS cfg (targets, mods) m = do
      compJSMod cfg m
      let infile = maybe (modInterfaceFile m) id (modSourceFile m)
          modpair = (modPackageKey m, infile)
      if modIsTarget m
        then return $ (modpair : targets, modName m : mods)
        else return (targets, modName m : mods)

    mkGhcCfg fs args = defaultConfig {
        cfgGhcFlags = fs,
        cfgGhcLibDir = Just hasteGhcLibDir,
        cfgUseTargetsFromFlags = True,
        cfgUseGhcErrorLogger = True,
        cfgUpdateDynFlags = \dfs -> dfs {
            ghcLink = NoLink,
            ghcMode = if "-c" `elem` args
                        then OneShot
                        else CompManager,
            hscTarget = HscAsm
          }
#if __GLASGOW_HASKELL__ >= 710
        , cfgCustomPrimIface = Just (primOpInfo, primOpStrictness)
#endif
      }
    mkLinkerCfg dfs extralibdirs cfg = cfg {
        mainMod = Just (pkgKeyString $ modulePkgKey (mainModIs dfs),
                        moduleNameString $ moduleName (mainModIs dfs)),
        libPaths = extralibdirs ++ libPaths cfg
      }
    setShowOutputable dfs cfg = cfg {showOutputable = showPpr dfs}

#if __GLASGOW_HASKELL__ >= 710
-- | Primop info for custom GHC.Prim interface.
primOpInfo :: PrimOp -> PrimOpInfo
#include "primop-info-710.hs"

-- | Strictness info for custom GHC.Prim interface.
primOpStrictness :: PrimOp -> Arity -> StrictSig
#include "primop-stricts-710.hs"
#endif

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
    booting <- maybe False (const True) `fmap` lookupEnv "HASTE_BOOTING"
    _ <- Sh.shell $ Sh.run_ ghcBinary (pkgargs booting ++ ghcArgs) ""
    return ()
  where
    Right (_, ghcArgs) = parseArgs hasteOpts "" args
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
  _ <- Sh.shell $ do
    pkgDirExists <- Sh.isDirectory pkgUserDir
    when (not pkgDirExists) $ do
      Sh.mkdir True pkgUserLibDir
      Sh.runInteractive hastePkgBinary ["init", pkgUserDir]
  return ()

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
   DontRun msg    -> Left $ putStrLn msg
   Run GHC        -> Left $ callVanillaGHC args
   Run Haste -> do
     case parseArgs hasteOpts helpHeader args of
       Left msg          -> Left $ putStrLn msg
       Right (cfg, rest) -> Right (filter (/= "-prof") rest, cfg)
   Run InstallJSExe -> do
     Left $ installJSExe (jsexeIn args) (jsexeOut args)
   Run BuildRunner -> do
     Left $ callVanillaGHC (rawargs ++ ["-package-id", "Cabal-1.23.0.0-f701f4ea98ec7bed5883c4df743045e6"])
   where
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
