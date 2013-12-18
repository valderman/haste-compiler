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
import System.IO.Temp
import Control.Monad
import qualified Codec.Archive.Zip as Zip
import Haste.Environment
import Haste.Version
import Control.Shell
import Data.Char (isDigit)

downloadFile :: String -> IO (Either String BS.ByteString)
downloadFile f = do
  (_, rsp) <- Network.Browser.browse $ do
    setAllowRedirects True
    request $ Request {
        rqURI = fromJust $ parseURI f,
        rqMethod = GET,
        rqHeaders = [],
        rqBody = BS.empty
      }
  case rspCode rsp of 
    (2, _, _) -> return $ Right $ rspBody rsp
    _         -> return $ Left $ rspReason rsp

data Cfg = Cfg {
    getLibs      :: Bool,
    getClosure   :: Bool,
    useLocalLibs :: Bool
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
      cfg = Cfg {
          getLibs      = libs,
          getClosure   = closure,
          useLocalLibs = local
        }

  when (needsReboot || forceBoot) $ do
    if local
      then bootHaste cfg "."
      else withSystemTempDirectory "haste" $ bootHaste cfg

bootHaste :: Cfg -> FilePath -> IO ()
bootHaste cfg tmpdir = do
  setCurrentDirectory tmpdir
  when (getLibs cfg) $ do
    when (not $ useLocalLibs cfg) $ do
      fetchLibs tmpdir
    exists <- doesDirectoryExist hasteInstDir
    when exists $ removeDirectoryRecursive hasteInstDir
    exists <- doesDirectoryExist jsmodDir
    when exists $ removeDirectoryRecursive jsmodDir
    exists <- doesDirectoryExist pkgDir
    when exists $ removeDirectoryRecursive pkgDir
    buildLibs
  when (getClosure cfg) $ do
    installClosure
  Prelude.writeFile bootFile (show bootVersion)

-- | Fetch the Haste base libs.
fetchLibs :: FilePath -> IO ()
fetchLibs tmpdir = do
    putStrLn "Downloading base libs from ekblad.cc"
    res <- downloadFile $ mkUrl hasteVersion
    case res of
      Left err ->
        error $ "Unable to download base libs: " ++ err
      Right f ->
        unpack tmpdir . read . decompress $ f
  where
    mkUrl v =
      "http://ekblad.cc/haste-libs/haste-libs-" ++ showVersion v ++ ".tar.bz2"

-- | Fetch and install the Closure compiler.
installClosure :: IO ()
installClosure = do
  putStrLn "Downloading Google Closure compiler..."
  closure <- downloadFile closureURI
  case closure of
    Left _ ->
      putStrLn "Couldn't download Closure compiler; continuing without."
    Right closure' -> do
      let cloArch = Zip.toArchive closure'
      case Zip.findEntryByPath "compiler.jar" cloArch of
        Just compiler ->
          BS.writeFile closureCompiler
                       (Zip.fromEntry compiler)
        _ ->
          putStrLn "Couldn't unpack Closure compiler; continuing without."
  where
    closureURI =
      "http://dl.google.com/closure-compiler/compiler-latest.zip"

-- | Build haste's base libs.
buildLibs :: IO ()
buildLibs = do
    res <- shell $ do
      -- Set up dirs and copy includes
      mkdir True $ pkgLibDir
      cpDir "include" hasteDir
      run_ hastePkgBinary ["update", "libraries" </> "rts.pkg"] ""
      
      inDirectory "libraries" $ do
        -- Install ghc-prim
        inDirectory "ghc-prim" $ do
          hasteInst ["configure"]
          hasteInst ["build", "--install-jsmods", ghcOpts]
          run_ hasteInstHisBinary ["ghc-prim-0.3.0.0", "dist" </> "build"] ""
          run_ hastePkgBinary ["update", "packageconfig"] ""
        
        -- Install integer-gmp; double install shouldn't be needed anymore.
        run_ hasteCopyPkgBinary ["Cabal"] ""
        inDirectory "integer-gmp" $ do
          hasteInst ["install", ghcOpts]
        
        -- Install base
        inDirectory "base" $ do
          basever <- file "base.cabal" >>= return
            . dropWhile (not . isDigit)
            . head
            . filter (not . null)
            . filter (and . zipWith (==) "version")
            . lines
          hasteInst ["configure"]
          hasteInst ["build", "--install-jsmods", ghcOpts]
          let base = "base-" ++ basever
              pkgdb = "--package-db=dist" </> "package.conf.inplace"
          run_ hasteInstHisBinary [base, "dist" </> "build"] ""
          run_ hasteCopyPkgBinary [base, pkgdb] ""
        
        -- Install array, fursuit and haste-lib
        forM_ ["array", "fursuit", "haste-lib"] $ \pkg -> do
          inDirectory pkg $ hasteInst ["install"]
    case res of
      Left err -> error err
      _        -> return ()
  where
    ghcOpts =
      "--ghc-options=-DHASTE_HOST_WORD_SIZE_IN_BITS=" ++ show hostWordSize
    hasteInst args =
      run_ hasteInstBinary ("--unbooted" : args) ""
