import Prelude hiding (read)
import System.Process
import System.Directory
import Network.Curl.Download.Lazy
import Network.Curl.Opts
import Data.ByteString.Lazy as BS hiding (putStrLn, unpack, elem)
import Data.Version
import Codec.Compression.BZip
import Codec.Archive.Tar
import System.Environment (getArgs)
import System.IO.Temp
import Control.Monad
import qualified Codec.Archive.Zip as Zip
import Haste.Environment
import Haste.Version

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
    _ <- system $ "bash ./buildlibs.sh " ++ show hostWordSize
    return ()
  when (getClosure cfg) $ do
    installClosure
  Prelude.writeFile bootFile (show bootVersion)

fetchLibs :: FilePath -> IO ()
fetchLibs tmpdir = do
    putStrLn "Downloading base libs from ekblad.cc"
    res <- openLazyURIWithOpts [CurlFollowLocation True] $ mkUrl hasteVersion
    case res of
      Left err ->
        error $ "Unable to download base libs: " ++ err
      Right file ->
        unpack tmpdir . read . decompress $ file
  where
    mkUrl v =
      "http://ekblad.cc/haste-libs/haste-libs-" ++ showVersion v ++ ".tar.bz2"

installClosure :: IO ()
installClosure = do
  putStrLn "Downloading Google Closure compiler..."
  closure <- openLazyURIWithOpts [CurlFollowLocation True] closureURI
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
