{-# LANGUAGE TupleSections #-}
-- | haste-install-his; install all .hi files in a directory.
module Main where
import Haste.Environment
import System.FilePath
import System.Directory
import System.Environment
import Control.Applicative
import Control.Monad
import Data.List
import Data.Char

main :: IO ()
main = do
  args <- getArgs
  case args of
    [package, dir] -> installFromDir (libDir </> package) dir
    _              -> putStrLn "Usage: haste-install-his pkgname dir"

getHiFiles :: FilePath -> IO [FilePath]
getHiFiles dir =
  filter (".hi" `isSuffixOf`) <$> getDirectoryContents dir

getSubdirs :: FilePath -> IO [FilePath]
getSubdirs dir = do
  contents <- getDirectoryContents dir
  someDirs <- mapM (\d -> (d,) <$> doesDirectoryExist (dir </> d)) contents
  return [path | (path, isDir) <- someDirs
               , isDir
               , head path /= '.'
               , isUpper (head path)]

installFromDir :: FilePath -> FilePath -> IO ()
installFromDir base path = do
  hiFiles <- getHiFiles path
  when (not $ null hiFiles) $ do
    createDirectoryIfMissing True (libDir </> base)
  mapM_ (installHiFile base path) hiFiles
  getSubdirs path >>= mapM_ (\d -> installFromDir (base </> d) (path </> d))

installHiFile :: FilePath -> FilePath -> FilePath -> IO ()
installHiFile to from file = do
  putStrLn $ "Installing " ++ from </> file ++ "..."
  copyFile (from </> file) (to </> file)
