{-# LANGUAGE CPP #-}
import Prelude hiding (read)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Version
import Data.List (foldl')
import Data.Maybe (fromJust)
import Codec.Compression.BZip
import Codec.Archive.Tar
import Haste.Environment
import Haste.Version
import Control.Shell
import Control.Shell.Concurrent
import Control.Shell.Download
import Data.Char (isDigit)
import Haste.Args
import System.Console.GetOpt
import GHC.Paths (libdir)
import System.Info (os)
import System.Directory (copyPermissions)

#if __GLASGOW_HASKELL__ >= 710
ghcMajor = "7.10"
libDir = "ghc-7.10"
primVersion = "0.4.0.0"
#else
ghcMajor = "7.8"
libDir = "ghc-7.8"
primVersion = "0.3.0.0"
#endif

data Cfg = Cfg {
    getLibs               :: Bool,
    getClosure            :: Bool,
    useLocalLibs          :: Bool,
    tracePrimops          :: Bool,
    forceBoot             :: Bool,
    initialPortableBoot   :: Bool,
    getHasteCabal         :: Bool,
    verbose               :: Bool
  }

defCfg :: Cfg
#ifdef PORTABLE
defCfg = Cfg {
    getLibs               = False,
    getClosure            = False,
    useLocalLibs          = False,
    tracePrimops          = False,
    forceBoot             = False,
    initialPortableBoot   = False,
    getHasteCabal         = True,
    verbose               = False
  }
#else
defCfg = Cfg {
    getLibs               = True,
    getClosure            = True,
    useLocalLibs          = False,
    tracePrimops          = False,
    forceBoot             = False,
    initialPortableBoot   = False,
    getHasteCabal         = True,
    verbose               = False
  }
#endif

devBoot :: Cfg -> Cfg
devBoot cfg = cfg {
    useLocalLibs          = True,
    forceBoot             = True,
    getClosure            = False,
    getHasteCabal         = False
  }

setInitialPortableBoot :: Cfg -> Cfg
setInitialPortableBoot cfg = cfg {
    getLibs             = True,
    useLocalLibs        = True,
    forceBoot           = True,
    getClosure          = True,
    initialPortableBoot = True,
    getHasteCabal       = True
  }

specs :: [OptDescr (Cfg -> Cfg)]
specs = [
#ifndef PORTABLE
      Option "" ["dev"]
           (NoArg devBoot) $
           "Boot Haste for development. Implies --force " ++
           "--local --no-closure --no-haste-cabal"
    , Option "" ["force"]
#else
      Option "" ["force"]
#endif
           (NoArg $ \cfg -> cfg {forceBoot = True}) $
           "Re-boot Haste even if already properly booted."
    , Option "" ["initial"]
           (NoArg setInitialPortableBoot) $
           "Prepare boot files for binary distribution. Should only ever " ++
           "be called by the release build scripts, never by users.\n" ++
           "Implies --local --force."
#ifndef PORTABLE
    , Option "" ["local"]
           (NoArg $ \cfg -> cfg {useLocalLibs = True}) $
           "Use libraries from source repository rather than " ++
           "downloading a matching set from the Internet. " ++
           "This is nearly always necessary when installing " ++
           "Haste from Git rather than from Hackage. " ++
           "When using --local, your current working directory " ++
           "must be the root of the Haste source tree."
    , Option "" ["no-closure"]
           (NoArg $ \cfg -> cfg {getClosure = False}) $
           "Don't download Closure compiler. You won't be able " ++
           "to use --opt-minify, unless you manually " ++
           "give hastec the path to compiler.jar."
    , Option "" ["no-haste-cabal"]
           (NoArg $ \cfg -> cfg {getHasteCabal = False}) $
           "Don't install haste-cabal. This is probably not " ++
           "what you want."
    , Option "" ["no-libs"]
           (NoArg $ \cfg -> cfg {getLibs = False}) $
           "Don't install any libraries. This is probably not " ++
           "what you want."
    , Option "" ["trace-primops"]
           (NoArg $ \cfg -> cfg {tracePrimops = True}) $
           "Build standard libs for tracing of primitive " ++
           "operations. Only use if you're debugging the code " ++
           "generator."
#endif
    , Option "v" ["verbose"]
           (NoArg $ \cfg -> cfg {verbose = True}) $
           "Print absolutely everything."
  ]

hdr :: String
hdr = "Fetch, build and install all libraries necessary to use Haste.\n"

data CabalOp = Configure | Build | Install | Clean

main :: IO ()
main = shell_ $ do
  when ("--help" `elem` cmdline || "-?" `elem` cmdline) $ do
    echo $ printHelp hdr specs
    exit

  case getOpt Permute specs cmdline of
    (cfgs, [], []) -> do
      let cfg = foldl' (flip (.)) id cfgs defCfg
      when (hasteNeedsReboot || forceBoot cfg) $ do
        if useLocalLibs cfg
          then bootHaste cfg "."
          else withTempDirectory "haste" $ bootHaste cfg
    (cfgs, nopts, errs) -> do
      let errors = errs ++ map (\x -> "unrecognized option `" ++ x ++ "'") nopts
      fail $ unlines errors

bootHaste :: Cfg -> FilePath -> Shell ()
bootHaste cfg tmpdir =
  withEnv "nodosfilewarning" (const "1") . inDirectory tmpdir $ do
    removeBootFile <- isFile bootFile
    when removeBootFile $ rm bootFile
    when (getLibs cfg) $ do
      -- Don't clear dir when it contains binaries; portable should only be built
      -- by scripts anyway, so this dir ought to be clean.
      when (not portableHaste) $ do
        mapM_ clearDir [pkgUserLibDir, jsmodUserDir, pkgUserDir,
                        pkgSysLibDir, jsmodSysDir, pkgSysDir]

      when (getHasteCabal cfg) $ do
        installHasteCabal portableHaste tmpdir

      -- Spawn off closure download in the background.
      closure <- future $ when (getClosure cfg) installClosure

      when (not $ useLocalLibs cfg) $ do
        fetchLibs tmpdir

      when (not portableHaste || initialPortableBoot cfg) $ do
        mkdir True hasteSysDir
        copyGhcSettings hasteSysDir
        void $ run hastePkgBinary ["init", pkgSysDir] ""
        buildLibs cfg

      when (initialPortableBoot cfg) $ do
        mapM_ relocate ["array", "bytestring", "containers", "base",
                        "deepseq", "dlist", "haste-prim", "time", "haste-lib",
                        "monads-tf", "old-locale", "transformers", "integer-gmp"]

      -- Wait for closure download to finish.
      await closure


    output bootFile (showBootVersion bootVersion)

clearDir :: FilePath -> Shell ()
clearDir dir = do
  exists <- isDirectory dir
  when exists $ rmdir dir

installHasteCabal :: Bool -> FilePath -> Shell ()
installHasteCabal portable tmpdir = do
    echo "Downloading haste-cabal from GitHub"
    f <- (decompress . BSL.fromChunks . (:[])) `fmap` fetchBytes hasteCabalUrl
    if os == "linux"
      then do
        mkdir True hasteCabalRootDir
        liftIO . unpack hasteCabalRootDir $ read f
        liftIO $ copyPermissions
                    hasteBinary
                    (hasteCabalRootDir </> "haste-cabal/haste-cabal.bin")
        output (hasteBinDir </> hasteCabalFile) launcher
      else do
        liftIO $ BSL.writeFile (hasteBinDir </> hasteCabalFile) f
    liftIO $ copyPermissions hasteBinary (hasteBinDir </> hasteCabalFile)
  where
    baseUrl = "http://valderman.github.io/haste-libs/"
    hasteCabalUrl
      | os == "linux" = baseUrl ++ "haste-cabal.linux.tar.bz2"
      | otherwise     = baseUrl ++ "haste-cabal" <.> os <.> "bz2"
    hasteCabalFile = "haste-cabal" ++ if os == "mingw32" then ".exe" else ""

    hasteCabalRootDir
      | portable  = hasteBinDir </> ".."
      | otherwise = hasteSysDir

    -- We need to determine the haste-cabal libdir at runtime if we're
    -- portable
    launcher
      | portable = unlines [
            "#!/bin/bash",
            "HASTEC=\"$(dirname $0)/hastec\"",
            "DIR=\"$($HASTEC --print-libdir)/../haste-cabal\"",
            "export LD_LIBRARY_PATH=$DIR",
            "exec $DIR/haste-cabal.bin $@"
          ]
      | otherwise = unlines [
            "#!/bin/bash",
            "DIR=\"" ++ hasteCabalRootDir </> "haste-cabal" ++ "\"",
            "export LD_LIBRARY_PATH=$DIR",
            "exec $DIR/haste-cabal.bin $@"
          ]

-- | Fetch the Haste base libs.
fetchLibs :: FilePath -> Shell ()
fetchLibs tmpdir = do
    echo "Downloading base libs from GitHub"
    file <- fetchBytes $ mkUrl hasteVersion
    liftIO . unpack tmpdir . read . decompress $ BSL.fromChunks [file]
  where
    mkUrl v =
      "http://valderman.github.io/haste-libs/haste-libs-" ++ showVersion v ++ ".tar.bz2"

-- | Fetch and install the Closure compiler.
installClosure :: Shell ()
installClosure = do
    echo "Downloading Google Closure compiler..."
    downloadClosure `orElse` do
      echo "Couldn't install Closure compiler; continuing without."
  where
    downloadClosure = do
      fetchBytes closureURI >>= (liftIO . BS.writeFile closureCompiler)
    closureURI =
      "http://valderman.github.io/haste-libs/compiler.jar"

-- | Build haste's base libs.
buildLibs :: Cfg -> Shell ()
buildLibs cfg = do
    -- Set up dirs and copy includes
    mkdir True $ pkgSysLibDir
    cpdir "include" hasteSysDir

    inDirectory ("utils" </> "unlit") $ do
      let out    = if os == "mingw32" then "unlit.exe" else "unlit"
          static = if os == "darwin" then [] else ["-static"]
          dash_s = if os == "darwin" then [] else ["-s"]
      run_ "gcc" (["-o" ++ out, "-O2", "unlit.c"]++static) ""
      run_ "strip" (dash_s ++ [out]) ""
      cp out (hasteSysDir </> out)

    run_ hastePkgBinary ["update", "--global", "libraries" </> "rts.pkg"] ""

    inDirectory "libraries" $ do
      inDirectory libDir $ do
        -- Install ghc-prim
        inDirectory "ghc-prim" $ do
          hasteCabal Install ["--solver", "topdown"]

          -- To get the GHC.Prim module in spite of pretending to have
          -- build-type: Simple
          let osxprim = if os == "darwin" then "-osx" else ""
          run_ hastePkgBinary ["unregister", "--global","ghc-prim"] ""
          run_ hastePkgBinary ["update", "--global",
                               "ghc-prim-"++primVersion++osxprim++".conf"] ""

        -- Install integer-gmp; double install shouldn't be needed anymore.
        inDirectory "integer-gmp" $ do
          hasteCabal Install ["--solver", "topdown"]

        -- Install base
        inDirectory "base" $ do
          hasteCabal Clean []
          hasteCabal Install ["--solver", "topdown", "-finteger-gmp"]

        -- Install array
        inDirectory "array" $ hasteCabal Clean []
        inDirectory "array" $ hasteCabal Install []

      -- Install haste-prim
      inDirectory "haste-prim" $ hasteCabal Install []

      -- Install time
      inDirectory "time" $ hasteCabal Install []

      -- Install haste-lib
      inDirectory "haste-lib" $ hasteCabal Install []

      -- Export monads-tf; it seems to be hidden by default
      run_ hastePkgBinary ["expose", "monads-tf"] ""
  where
    ghcOpts = concat [
        if tracePrimops cfg then ["--hastec-option=-debug"] else [],
        if verbose cfg then ["--verbose"] else []]
    configOpts = [ "--with-hastec=" ++ hasteBinary
                 , "--with-haste-pkg=" ++ hastePkgBinary
                 , "--libdir=" ++ if os == "darwin"
                                    then pkgSysLibDir
                                    else takeDirectory pkgSysLibDir
                 , "--package-db=clear"
                 , "--package-db=global"
                 , "--hastec-option=-fforce-recomp"
#if __GLASGOW_HASKELL__ < 709
                 , "--hastec-option=-DHASTE_HOST_WORD_SIZE_IN_BITS=" ++
                    show hostWordSize
#endif
                 ]
    hasteCabal Configure args =
      withEnv "HASTE_BOOTING" (const "1") $ run_ hasteCabalBinary as ""
      where as = "configure" : args ++ ghcOpts ++ configOpts
    hasteCabal Install args =
      withEnv "HASTE_BOOTING" (const "1") $ run_ hasteCabalBinary as ""
      where as = "install" : args ++ ghcOpts ++ configOpts
    hasteCabal Build args =
      withEnv "HASTE_BOOTING" (const "1") $ run_ hasteCabalBinary as ""
      where as = "build" : args ++ ghcOpts
    hasteCabal Clean args =
      withEnv "HASTE_BOOTING" (const "1") $ run_ hasteCabalBinary as ""
      where as = "clean" : args

    vanillaCabal args = run_ "cabal" args ""


-- | Copy GHC settings and utils into the given directory.
copyGhcSettings :: FilePath -> Shell ()
copyGhcSettings dest = do
  cp (libdir </> "platformConstants") (dest </> "platformConstants")
#ifdef mingw32_HOST_OS
  cp ("settings-ghc-" ++ ghcMajor ++ ".windows") (dest </> "settings")
  cp (libdir </> "touchy.exe") (dest </> "touchy.exe")
#else
  cp (libdir </> "settings") (dest </> "settings")
#endif

relocate :: String -> Shell ()
relocate pkg = run_ hastePkgBinary ["relocate", pkg] ""
