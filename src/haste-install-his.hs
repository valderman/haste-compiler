{-# LANGUAGE TupleSections #-}
-- | haste-install-his; install all .hi files in a directory.
module Main where
import Haste.Environment
import System.Environment
import Control.Applicative
import Control.Monad
import Data.List
import Data.Char
import Control.Shell

main :: IO ()
main = do
  args <- getArgs
  case args of
    [package, dir] -> shell $ installFromDir (pkgSysLibDir </> package) dir
    _              -> shell $ echo "Usage: haste-install-his pkgname dir"
  return ()

getHiFiles :: FilePath -> Shell [FilePath]
getHiFiles dir =
  filter (".hi" `isSuffixOf`) <$> ls dir

getSubdirs :: FilePath -> Shell [FilePath]
getSubdirs dir = do
  contents <- ls dir
  someDirs <- mapM (\d -> (d,) <$> isDirectory (dir </> d)) contents
  return [path | (path, isDir) <- someDirs
               , isDir
               , head path /= '.'
               , isUpper (head path)]

installFromDir :: FilePath -> FilePath -> Shell ()
installFromDir base path = do
  hiFiles <- getHiFiles path
  when (not $ null hiFiles) $ do
    mkdir True (pkgSysLibDir </> base)
  mapM_ (installHiFile base path) hiFiles
  getSubdirs path >>= mapM_ (\d -> installFromDir (base </> d) (path </> d))

installHiFile :: FilePath -> FilePath -> FilePath -> Shell ()
installHiFile to from file = do
  echo $ "Installing " ++ from </> file ++ "..."
  cp (from </> file) (to </> file)
