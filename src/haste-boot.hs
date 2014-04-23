{-# LANGUAGE CPP #-}
import Prelude hiding (read)
import System.Directory
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
import qualified Codec.Archive.Zip as Zip
import Haste.Environment
import Haste.Version
import Control.Shell
import Data.Char (isDigit)
import Control.Monad.IO.Class (liftIO)
import Args

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
    getLibs      :: Bool,
    getClosure   :: Bool,
    useLocalLibs :: Bool,
    tracePrimops :: Bool,
    forceBoot    :: Bool
  }

defCfg :: Cfg
defCfg = Cfg {
    getLibs = True,
    getClosure = True,
    useLocalLibs = False,
    tracePrimops = False,
    forceBoot = False
  }

specs :: [ArgSpec Cfg]
specs = [
    ArgSpec { optName = "force",
              updateCfg = \cfg _ -> cfg {forceBoot = True},
              info = "Re-boot Haste even if already properly booted."},
    ArgSpec { optName = "local",
              updateCfg = \cfg _ -> cfg {useLocalLibs = True},
              info = "Use libraries from source repository rather than " ++
                     "downloading a matching set from the Internet. " ++
                     "This is nearly always necessary when installing " ++
                     "Haste from Git rather than from Hackage. " ++
                     "When using --local, your current working directory " ++
                     "must be the root of the Haste source tree."},
    ArgSpec { optName = "no-closure",
              updateCfg = \cfg _ -> cfg {getClosure = False},
              info = "Don't download Closure compiler. You won't be able " ++
                     "to use --opt-google-closure, unless you manually " ++
                     "give it the path to compiler.jar."},
    ArgSpec { optName = "no-libs",
              updateCfg = \cfg _ -> cfg {getLibs = False},
              info = "Don't install any libraries. This is probably not " ++
                     "what you want."},
    ArgSpec { optName = "trace-primops",
              updateCfg = \cfg _ -> cfg {tracePrimops = True},
              info = "Build standard libs for tracing of primitive " ++
                     "operations. Only use if you're debugging the code " ++
                     "generator."}
  ]

main :: IO ()
main = do
  args <- getArgs
  case handleArgs defCfg specs args of
    Right (cfg, _) -> do
      when (needsReboot || forceBoot cfg) $ do
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
    when (not $ useLocalLibs cfg) $ do
      fetchLibs tmpdir
    exists <- isDirectory hasteInstDir
    when exists $ rmdir hasteInstDir
    exists <- isDirectory jsmodDir
    when exists $ rmdir jsmodDir
    exists <- isDirectory pkgDir
    when exists $ rmdir pkgDir
    buildLibs cfg
  when (getClosure cfg) $ do
    installClosure
  file bootFile (show bootVersion)

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
    downloadAndUnpackClosure `orElse` do
      echo "Couldn't install Closure compiler; continuing without."
  where
    downloadAndUnpackClosure = do
      file <- downloadFile closureURI
      let cloArch = Zip.toArchive file
      case Zip.findEntryByPath "compiler.jar" cloArch of
        Just compiler ->
          liftIO $ BS.writeFile closureCompiler (Zip.fromEntry compiler)
        _ ->
          fail "Unable to unpack Closure compiler"
    closureURI =
      "http://dl.google.com/closure-compiler/compiler-latest.zip"

-- | Build haste's base libs.
buildLibs :: Cfg -> Shell ()
buildLibs cfg = do
    -- Set up dirs and copy includes
    mkdir True $ pkgLibDir
    cpDir "include" hasteDir
    run_ hastePkgBinary ["update", "libraries" </> "rts.pkg"] ""
    
    inDirectory "libraries" $ do
      -- Install ghc-prim
      inDirectory "ghc-prim" $ do
        hasteInst ["configure", "--solver", "topdown"]
        hasteInst $ ["build", "--install-jsmods"] ++ ghcOpts
        run_ hasteInstHisBinary ["ghc-prim-0.3.0.0", "dist" </> "build"] ""
        run_ hastePkgBinary ["update", "packageconfig"] ""
      
      -- Install integer-gmp; double install shouldn't be needed anymore.
      run_ hasteCopyPkgBinary ["Cabal"] ""
      inDirectory "integer-gmp" $ do
        hasteInst ("install" : "--solver" : "topdown" : ghcOpts)
      
      -- Install base
      inDirectory "base" $ do
        basever <- file "base.cabal" >>= return
          . dropWhile (not . isDigit)
          . head
          . filter (not . null)
          . filter (and . zipWith (==) "version")
          . lines
        hasteInst ["configure", "--solver", "topdown"]
        hasteInst $ ["build", "--install-jsmods"] ++ ghcOpts
        let base = "base-" ++ basever
            pkgdb = "--package-db=dist" </> "package.conf.inplace"
        run_ hasteInstHisBinary [base, "dist" </> "build"] ""
        run_ hasteCopyPkgBinary [base, pkgdb] ""
      
      -- Install array, fursuit and haste-lib
      forM_ ["array", "fursuit", "haste-lib"] $ \pkg -> do
        inDirectory pkg $ hasteInst ("install" : ghcOpts)
  where
    ghcOpts = concat [
        if tracePrimops cfg then ["--ghc-option=-debug"] else [],
        ["--ghc-option=-DHASTE_HOST_WORD_SIZE_IN_BITS=" ++ show hostWordSize]
      ]
    hasteInst args =
      run_ hasteInstBinary ("--unbooted" : args) ""
