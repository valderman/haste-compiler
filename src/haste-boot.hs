{-# LANGUAGE CPP #-}
import Prelude hiding (read)
import Network.HTTP
import Network.Browser hiding (err)
import Network.URI
import qualified Data.ByteString.Lazy as BS
import Data.Version
import Data.Maybe (fromJust)
import Codec.Compression.BZip
import Codec.Archive.Tar
import System.Environment (getArgs)
import System.Exit
import Control.Monad
import Haste.Environment
import Haste.Version
import Control.Shell
import Data.Char (isDigit)
import Control.Monad.IO.Class (liftIO)
import Haste.Args
import System.Console.GetOpt
import GHC.Paths (libdir)
import System.Info (os)
import System.Directory (copyPermissions)
import System.FilePath (takeDirectory)

#if __GLASGOW_HASKELL__ >= 710
libDir = "ghc-7.10"
primVersion = "0.4.0.0"
#else
libDir = "ghc-7.8"
primVersion = "0.3.0.0"
#endif

downloadFile :: String -> Shell BS.ByteString
downloadFile f = do
  (_, rsp) <- liftIO $ Network.Browser.browse $ do
    setAllowRedirects True
    request $ Request {
        rqURI = fromJust $ parseURI f,
        rqMethod = GET,
        rqHeaders = [],
        rqBody = BS.empty
      }
  case rspCode rsp of 
    (2, _, _) -> return $ rspBody rsp
    _         -> fail $ "Failed to download " ++ f ++ ": " ++ rspReason rsp

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
    , Option "v" ["verbose"]
           (NoArg $ \cfg -> cfg {verbose = True}) $
           "Print absolutely everything."
#endif
  ]

hdr :: String
hdr = "Fetch, build and install all libraries necessary to use Haste.\n"

data CabalOp = Configure | Build | Install | Clean

main :: IO ()
main = do
  args <- getArgs
  case parseArgs specs hdr args of
    Right (mkConfig, _) -> do
      let cfg = mkConfig defCfg
      when (hasteNeedsReboot || forceBoot cfg) $ do
        res <- shell $ if useLocalLibs cfg
                         then bootHaste cfg "."
                         else withTempDirectory "haste" $ bootHaste cfg
        case res of
          Right _  -> return ()
          Left err -> putStrLn err >> exitFailure
    Left halp -> do
      putStrLn halp

bootHaste :: Cfg -> FilePath -> Shell ()
bootHaste cfg tmpdir = inDirectory tmpdir $ do
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

    when (not $ useLocalLibs cfg) $ do
      fetchLibs tmpdir

    when (not portableHaste || initialPortableBoot cfg) $ do
      mkdir True hasteSysDir
      copyGhcSettings hasteSysDir
      void $ run hastePkgBinary ["init", pkgSysDir] ""
      buildLibs cfg

    when (initialPortableBoot cfg) $ do
      mapM_ relocate ["array", "bytestring", "containers",
                      "deepseq", "dlist", "haste-prim", "time", "haste-lib",
                      "monads-tf", "old-locale", "transformers", "integer-gmp"]

  when (getClosure cfg) $ do
    installClosure
  file bootFile (showBootVersion bootVersion)

clearDir :: FilePath -> Shell ()
clearDir dir = do
  exists <- isDirectory dir
  when exists $ rmdir dir

installHasteCabal :: Bool -> FilePath -> Shell ()
installHasteCabal portable tmpdir = do
    echo "Downloading haste-cabal from GitHub"
    f <- decompress `fmap` downloadFile hasteCabalUrl
    if os == "linux"
      then do
        mkdir True hasteCabalRootDir
        liftIO . unpack hasteCabalRootDir $ read f
        liftIO $ copyPermissions
                    hasteBinary
                    (hasteCabalRootDir </> "haste-cabal/haste-cabal.bin")
        file (hasteBinDir </> hasteCabalFile) launcher
      else do
        liftIO $ BS.writeFile (hasteBinDir </> hasteCabalFile) f
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
            "DIR=\"$(dirname $0)/../haste-cabal\"",
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
    file <- downloadFile $ mkUrl hasteVersion
    liftIO . unpack tmpdir . read . decompress $ file
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
      downloadFile closureURI >>= (liftIO . BS.writeFile closureCompiler)
    closureURI =
      "http://valderman.github.io/haste-libs/compiler.jar"

-- | Build haste's base libs.
buildLibs :: Cfg -> Shell ()
buildLibs cfg = do
    -- Set up dirs and copy includes
    mkdir True $ pkgSysLibDir
    cpDir "include" hasteSysDir

    inDirectory ("utils" </> "unlit") $ do
      let out = if os == "mingw32" then "unlit.exe" else "unlit"
      run_ "gcc" ["-o" ++ out, "-O2", "unlit.c", "-static"] ""
      run_ "strip" ["-s", out] ""
      cp out (hasteSysDir </> out)

    run_ hastePkgBinary ["update", "--global", "libraries" </> "rts.pkg"] ""

    inDirectory "libraries" $ do
      inDirectory libDir $ do
        -- Install ghc-prim
        inDirectory "ghc-prim" $ do
#if __GLASGOW_HASKELL__ >= 710
          cp "../../../include/ghcplatform.h" "../ghc_boot_platform.h"
          run_ "cpp" ["-P", "-I../../../include",
                      "../primops.txt.pp", "-o", "primops.txt"] ""
#endif
          hasteCabal Install ["--solver", "topdown"]

          -- To get the GHC.Prim module in spite of pretending to have
          -- build-type: Simple
          run_ hastePkgBinary ["unregister", "--global","ghc-prim"] ""
          run_ hastePkgBinary ["update",
                               "--global","ghc-prim-"++primVersion++".conf"] ""

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
                 , "--libdir=" ++ takeDirectory pkgSysLibDir
                 , "--package-db=clear"
                 , "--package-db=global"
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
  cp ("settings.windows") (dest </> "settings")
  cp (libdir </> "touchy.exe") (dest </> "touchy.exe")
#else
  cp (libdir </> "settings") (dest </> "settings")
#endif

relocate :: String -> Shell ()
relocate pkg = run_ hastePkgBinary ["relocate", pkg] ""
