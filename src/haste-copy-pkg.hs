-- | haste-copy-pkg; copy a package from file or GHC package DB and fix up its
--   paths.
module Main where
import System.Directory
import System.FilePath
import System.Process
import System.Environment
import System.IO
import Control.Concurrent
import Data.List
import Haste.Environment

main :: IO ()
main = do
  args <- getArgs
  let (pkgdbs, pkgs) = partition ("--package-db=" `isPrefixOf`) args
  if null args
    then putStrLn "Usage: haste-copy-pkg [--package-db=foo.conf] <packages>"
    else mapM_ (copyPkgConfig pkgdbs hastePkgBinary) pkgs

-- | Copy and modify a package config to work with Haste. A config can be
--   read from a file or copied from the system's package DB.
copyPkgConfig :: [String] -> FilePath -> String -> IO ()
copyPkgConfig pkgdbs hastepkg pkg = do
  isFile <- doesFileExist pkg
  if isFile
    then copyFromFile hastepkg pkg
    else copyFromDB pkgdbs hastepkg pkg

-- | Copy and modify a package config from a file.
copyFromFile :: FilePath -> FilePath -> IO ()
copyFromFile hastepkg pkgfile = do
  hPkgFile <- openFile pkgfile ReadMode
  sync <- newEmptyMVar
  savePkgConfig hastepkg pkgfile hPkgFile sync
  takeMVar sync
  return ()

-- | Copy a modified package config from the system's DB.
copyFromDB :: [String] -> FilePath -> String -> IO ()
copyFromDB pkgdbs hastepkg package = do
  (_, pkgdata, _, ghcpkgProc) <- runInteractiveProcess "ghc-pkg"
                                                       args
                                                       Nothing
                                                       Nothing
  hastepkgDone <- newEmptyMVar
  _ <- forkIO $ savePkgConfig hastepkg package pkgdata hastepkgDone 
  _ <- waitForProcess ghcpkgProc
  takeMVar hastepkgDone
  return ()
  where
    args = ["describe", package] ++ pkgdbs

-- | Modify and save a config read from a handle.
savePkgConfig :: FilePath -> String -> Handle -> MVar () -> IO ()
savePkgConfig hastepkg pkgname pkgdata hastepkgDone = do
  (indata, _, _, hpkgProc) <- runInteractiveProcess hastepkg
                                                    ["update", "-", "--force"]
                                                    Nothing
                                                    Nothing
  pkgtext <- hGetContents pkgdata
  hPutStrLn indata (fixPaths pkgname pkgtext)
  hClose indata
  _ <- waitForProcess hpkgProc
  putMVar hastepkgDone ()

-- | Hack a config to work with Haste.
fixPaths :: String -> String -> String
fixPaths pkgname pkgtext =
  pkgtext'
  where
    pkgtext' = unlines
             . map fixPath
             . filter (not . ("haddock" `isPrefixOf`))
             . filter (not . ("hs-libraries:" `isPrefixOf`))
             $ lines pkgtext
    
    fixPath str
      | isKey "library-dirs:" str =
        "library-dirs: " ++ importDir </> pkgname
      | isKey "import-dirs:" str =
        "import-dirs: " ++ importDir </> pkgname
      | isKey "pkgroot:" str =
        "pkgroot: \"" ++ pkgRoot ++ "\""
      | "-inplace" `isSuffixOf` str =
        reverse $ drop (length "-inplace") $ reverse str
      | otherwise =
        str

    isKey _ "" =
      False
    isKey key str =
      and $ zipWith (==) key str
    
    importDir = libDir
    pkgRoot   = hasteInstDir
