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
    tracePrimops :: Bool
  }

main :: IO ()
main = do
  args <- getArgs
  -- Always get base and closure when forced unless explicitly asked not to;
  -- if not forced, get base and closure when necessary, unless asked not to.
  let forceBoot = elem "--force" args
      libs      = if elem "--no-libs" args
                     then False
                     else forceBoot || needsReboot
      closure   = if elem "--no-closure" args
                     then False
                     else forceBoot || needsReboot
      local     = elem "--local" args
      trace     = elem "--trace-primops" args
      cfg = Cfg {
          getLibs      = libs,
          getClosure   = closure,
          useLocalLibs = local,
          tracePrimops = trace
        }

  when (needsReboot || forceBoot) $ do
    res <- shell $ if local
                     then bootHaste cfg "."
                     else withTempDirectory "haste" $ bootHaste cfg
    case res of
      Right _  -> return ()
      Left err -> putStrLn err >> exitFailure

bootHaste :: Cfg -> FilePath -> Shell ()
bootHaste cfg tmpdir = inDirectory tmpdir $ do
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
        hasteInst ["configure"]
        hasteInst $ ["build", "--install-jsmods"] ++ ghcOpts
        run_ hasteInstHisBinary ["ghc-prim-0.3.0.0", "dist" </> "build"] ""
        run_ hastePkgBinary ["update", "packageconfig"] ""
      
      -- Install integer-gmp; double install shouldn't be needed anymore.
      run_ hasteCopyPkgBinary ["Cabal"] ""
      inDirectory "integer-gmp" $ do
        hasteInst ("install" : ghcOpts)
      
      -- Install base
      inDirectory "base" $ do
        basever <- file "base.cabal" >>= return
          . dropWhile (not . isDigit)
          . head
          . filter (not . null)
          . filter (and . zipWith (==) "version")
          . lines
        hasteInst ["configure"]
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
