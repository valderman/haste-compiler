{-# LANGUAGE CPP #-}
-- | Haste's main compiler driver.
module Main where
import Language.Haskell.GHC.Simple
import GHC
import Outputable (showPpr)

import System.Environment (getArgs, lookupEnv)
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
    booting <- maybe False (const True) `fmap` lookupEnv "HASTE_BOOTING"
    let args = "-O2":concat [packageDBArgs,as,["-D__HASTE__="++show intVersion]]
        prof = "-prof" `elem` args
    case parseHasteFlags booting args of
      Left act             -> act
      Right (fs, mkConfig) -> do
        let ghcconfig = mkGhcCfg fs args
        (dfs, _) <- getDynFlagsForConfig ghcconfig
        let cfg = mkLinkerCfg dfs . setShowOutputable dfs $ mkConfig def
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
    -- TODO: this would be handled more elegantly modifying the dynflags
    --       directly
    packageDBArgs = ["-no-global-package-db",
                     "-no-user-package-db",
                     "-package-db " ++ pkgSysDir,
                     "-package-db " ++ pkgUserDir ]

    dotToSlash '.' = '/'
    dotToSlash c   = c

    buildJSLib profiling cfg pkgkey mods = do
      let basepath    = targetLibPath cfg
          modpath     = targetLibPath cfg ++ "/" ++ pkgkey
          mods'       = [ modpath ++ "/" ++ map dotToSlash mn ++ ".jsmod"
                        | mn <- mods]
          libfile     = concat [basepath,"/","libHS",pkgkey,".jslib"]
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
            hscTarget = HscNothing
          }

      }
    mkLinkerCfg dfs cfg = cfg {
        mainMod = Just (pkgKeyString $ modulePkgKey (mainModIs dfs),
                        moduleNameString $ moduleName (mainModIs dfs))
      }
    setShowOutputable dfs cfg = cfg {showOutputable = showPpr dfs}

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
    Right (_, ghcArgs) = parseArgs hasteOpts "" args

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
data Compiler = Haste | GHC | InstallJSExe deriving (Show, Eq)
data RunMode = Run !Compiler | DontRun !Message deriving (Show, Eq)

rebootMsg :: Message
rebootMsg = "Haste needs to be rebooted; please run haste-boot"

-- | How should we run the compiler for this command line?
runMode :: Bool -> [String] -> RunMode
runMode booting as
  | "--help" `elem` as                    = Run Haste
  | "--install-executable" `elem` as      = Run InstallJSExe
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
parseHasteFlags :: Bool -> [String] -> Either (IO ()) ([String], Config->Config)
parseHasteFlags booting args = do
  case runMode booting args of
   DontRun msg    -> Left $ putStrLn msg
   Run GHC        -> Left $ callVanillaGHC args
   Run Haste -> do
     case parseArgs hasteOpts helpHeader args of
       Left msg          -> Left $ putStrLn msg
       Right (cfg, rest) -> Right (filter (/= "-prof") rest, cfg)
   Run InstallJSExe -> do
     Left $ installJSExe (jsexeIn args) (jsexeOut args)
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
