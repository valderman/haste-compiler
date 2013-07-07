{-# LANGUAGE CPP #-}
import Prelude hiding (read)
import System.Directory
#ifdef DOWNLOAD_CURL
import Network.Curl.Download.Lazy
import Network.Curl.Opts
#else
import Network.Download
#endif
import Data.ByteString.Lazy as BS hiding
  (putStrLn, unpack, elem, filter, zipWith, null, head, dropWhile)
import Data.Version
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

downloadFile :: String -> IO (Either String ByteString)
#ifdef DOWNLOAD_CURL
downloadFile f = do
  openLazyURIWithOpts [CurlFollowLocation True] f
#else
downloadFile f = do
  bs <- openURI f
  return $ fmap (\bs' -> BS.fromChunks [bs']) bs
#endif

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
    exists <- doesDirectoryExist hasteDir
    when exists $ do
      removeDirectoryRecursive hasteDir
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
      "http://closure-compiler.googlecode.com/files/compiler-latest.zip"

-- | Build haste's base libs.
buildLibs :: IO ()
buildLibs = do
    res <- shell $ do
      -- Set up dirs and copy includes
      mkdir True $ pkgLibDir
      cpDir "include" hasteDir
      run_ "haste-pkg" ["update", "libraries" </> "rts.pkg"] ""
      
      inDirectory "libraries" $ do
        -- Install ghc-prim
        inDirectory "ghc-prim" $ do
          hasteInst ["configure"]
          hasteInst ["build", "--install-jsmods", ghcOpts]
          run_ "haste-install-his" ["ghc-prim-0.3.0.0", "dist" </> "build"] ""
          run_ "haste-pkg" ["update", "packageconfig"] ""
        
        -- Install integer-gmp twice, since it may misbehave the first time.
        inDirectory "integer-gmp" $ do
          hasteInst ["install", ghcOpts]
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
          run_ "haste-install-his" [base, "dist" </> "build"] ""
          run_ "haste-copy-pkg" [base, pkgdb] ""
        
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
      run_ "haste-inst" ("--unbooted" : args) ""
