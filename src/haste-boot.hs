import Prelude hiding (read)
import HasteReboot
import System.Exit (ExitCode (..))
import System.Process (waitForProcess, runProcess)
import System.Directory
import Network.Download
import Data.ByteString.Lazy as BS hiding (putStrLn, unpack, elem)
import Codec.Compression.BZip
import Codec.Archive.Tar
import System.Environment (getArgs)
import Control.Monad
import qualified Codec.Archive.Zip as Zip

data Cfg = Cfg {
    fetchBase :: Bool,
    fetchClosure  :: Bool
  }

main = do
  args <- getArgs
  let forceBoot = elem "--force" args
      base      = not $ elem "--no-base" args || needsReboot /= Everything
      closure   = not $ elem "--no-closure" args || needsReboot /= Everything
      cfg = Cfg {
          fetchBase    = base,
          fetchClosure = closure
        }
  cabalDir <- getAppUserDataDirectory "cabal"
  hasteDir <- getAppUserDataDirectory "haste"

  when (needsReboot /= Dont || forceBoot) $ do
    let localHastec = cabalDir ++ "/bin/hastec"
    mhastec <- locateCompiler ["hastec", localHastec]
    case mhastec of
      Nothing     -> return ()
      Just hastec -> bootHaste cfg hastec hasteDir

bootHaste :: Cfg -> FilePath -> FilePath -> IO ()
bootHaste cfg hastec hasteDir = do
  when (fetchBase cfg) $ fetchStdLibs hasteDir
  buildFursuit hastec hasteDir
  buildStdLib hastec hasteDir
  when (fetchClosure cfg) $ installClosure hasteDir
  Prelude.writeFile (hasteDir ++ "/booted") (show bootVer)

locateCompiler :: [FilePath] -> IO (Maybe FilePath)
locateCompiler (c:cs) = do
  exe <- findExecutable c
  case exe of
    Nothing -> locateCompiler cs
    _       -> return exe
locateCompiler _ = do
  putStrLn "No hastec executable found; aborting!"
  return Nothing

fetchStdLibs :: FilePath -> IO ()
fetchStdLibs hasteDir = do
  putStrLn "Downloading base libs from ekblad.cc"
  res <- openURI "http://ekblad.cc/haste-libs.tar.bz2"
  case res of
    Left err ->
      error $ "Unable to download base libs: " ++ err
    Right file ->
      unpack (hasteDir ++ "/..") . read . decompress $ fromChunks [file]

buildFursuit :: FilePath -> FilePath -> IO ()
buildFursuit hastec hasteDir = do
  let furDir = hasteDir ++ "/fursuit/src"
  gitGet hasteDir "fursuit" "git://github.com/valderman/fursuit.git"
  install hastec furDir ["FRP.Fursuit", "FRP.Fursuit.Async"]

buildStdLib :: FilePath -> FilePath -> IO ()
buildStdLib hastec hasteDir = do
  let builddir = hasteDir ++ "/hastesrc/src"
  gitGet hasteDir "hastesrc" "git://github.com/valderman/haste-compiler.git"
  install hastec builddir ["Haste", "Haste.Reactive", "Haste.JSON",
                           "Haste.Ajax", "Haste.Reactive.Ajax"]

installClosure :: FilePath -> IO ()
installClosure hasteDir = do
  putStrLn "Downloading Google Closure compiler..."
  closure <- openURI closureURI
  case closure of
    Left err ->
      putStrLn "Couldn't download Closure compiler; continuing without."
    Right closure' -> do
      let cloArch = Zip.toArchive $ fromChunks [closure']
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
  let clonedir = dir ++ "/" ++ repodir
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

install :: FilePath -> FilePath -> [FilePath] -> IO ()
install hastec srcdir modules = do
  putStrLn $ "Installing modules: " ++ show modules
  let args = ["-O2", "-Wall", "--libinstall", "--unbooted"] ++ modules
  build <- runProcess hastec args (Just srcdir) Nothing Nothing Nothing Nothing
  res <- waitForProcess build
  case res of
    ExitFailure _ -> error $ "Failed!"
    _             -> putStrLn "OK!"
