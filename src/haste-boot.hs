import Prelude hiding (read)
import Version
import System.Exit (ExitCode (..))
import System.Process
import System.FilePath
import System.Directory
import Network.Curl.Download.Lazy
import Network.Curl.Opts
import Data.ByteString.Lazy as BS hiding (putStrLn, unpack, elem)
import Codec.Compression.BZip
import Codec.Archive.Tar
import System.Environment (getArgs)
import Control.Monad
import qualified Codec.Archive.Zip as Zip
import EnvUtils

data Cfg = Cfg {
    fetchBase :: Bool,
    fetchClosure  :: Bool
  }

main :: IO ()
main = do
  args <- getArgs
  -- Always get base and closure when forced unless explicitly asked not to;
  -- if not forced, get base and closure when necessary, unless asked not to.
  let forceBoot = elem "--force" args
      base      = if elem "--no-base" args
                     then False
                     else forceBoot || needsReboot == Everything
      closure   = if elem "--no-closure" args
                     then False
                     else forceBoot || needsReboot == Everything
      cfg = Cfg {
          fetchBase    = base,
          fetchClosure = closure
        }

  when (needsReboot /= Dont || forceBoot) $ do
    let localHasteinst = cabalDir </> "bin" </> "haste-inst"
    mhasteinst <- locateCompiler ["haste-inst", localHasteinst]
    case mhasteinst of
      Just hasteinst -> bootHaste cfg hasteinst
      _              -> return ()

bootHaste :: Cfg -> FilePath -> IO ()
bootHaste cfg hasteinst = do
  when (fetchBase cfg) fetchStdLibs
  buildFursuit hasteinst
  buildStdLib hasteinst
  when (fetchClosure cfg) installClosure
  Prelude.writeFile (hasteDir </> "booted") (show bootVer)

fetchStdLibs :: IO ()
fetchStdLibs = do
  putStrLn "Downloading base libs from ekblad.cc"
  res <- openLazyURIWithOpts [CurlFollowLocation True] "http://ekblad.cc/haste-libs.tar.bz2"
  case res of
    Left err ->
      error $ "Unable to download base libs: " ++ err
    Right file ->
      unpack (hasteDir </> "..") . read . decompress $ file

buildFursuit :: FilePath -> IO ()
buildFursuit hastec = do
  let furDir = hasteDir </> "fursuit"
  gitGet hasteDir "fursuit" "git://github.com/valderman/fursuit.git"
  install hastec furDir

buildStdLib :: FilePath -> IO ()
buildStdLib hastec = do
  let builddir = hasteDir </> "hastesrc" </> "haste-lib"
  gitGet hasteDir "hastesrc" "git://github.com/valderman/haste-compiler.git"
  install hastec builddir

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
          BS.writeFile (hasteDir ++ "/compiler.jar")
                       (Zip.fromEntry compiler)
        _ ->
          putStrLn "Couldn't unpack Closure compiler; continuing without."
  where
    closureURI =
      "http://closure-compiler.googlecode.com/files/compiler-latest.zip"

gitGet :: FilePath -> FilePath -> String -> IO ()
gitGet dir repodir repo = do
  let clonedir = dir </> repodir
  putStrLn $ "Attempting to clone/update: " ++ repo
  exists <- doesDirectoryExist clonedir
  let (args, workdir) =
        if exists
          then (["pull"], clonedir)
          else (["clone", repo, repodir], dir)
  git <- runProcess "git" args (Just workdir) Nothing Nothing Nothing Nothing
  res <- waitForProcess git
  case res of
    ExitFailure _ -> error $ "Failed!"
    _             -> return ()

install :: FilePath -> FilePath -> IO ()
install hasteinst srcdir = do
  putStrLn $ "Running haste-inst in " ++ srcdir
  let args = ["install", "--unbooted"]
  build <- runProcess hasteinst args (Just srcdir)
                      Nothing Nothing Nothing Nothing
  res <- waitForProcess build
  case res of
    ExitFailure _ -> error $ "Failed!"
    _             -> putStrLn "OK!"
