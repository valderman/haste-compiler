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
import EnvUtils

main :: IO ()
main = do
  pkgs <- getArgs
  Just hastepkg <- locateCompiler ["haste-pkg",
                                   cabalDir </> "bin" </> "haste-pkg"]
  if null pkgs
    then putStrLn "Usage: haste-copy-pkg [--package-conf=foo.conf] <packages>"
    else mapM_ (copyPkgConfig hastepkg) pkgs

-- | Copy and modify a package config to work with Haste. A config can be
--   read from a file or copied from the system's package DB.
copyPkgConfig :: FilePath -> String -> IO ()
copyPkgConfig hastepkg pkg = do
  isFile <- doesFileExist pkg
  if isFile
    then copyFromFile hastepkg pkg
    else copyFromDB hastepkg pkg

-- | Copy and modify a package config from a file.
copyFromFile :: FilePath -> FilePath -> IO ()
copyFromFile hastepkg pkgfile = do
  hPkgFile <- openFile pkgfile ReadMode
  sync <- newEmptyMVar
  savePkgConfig hastepkg hPkgFile sync
  takeMVar sync
  return ()

-- | Copy a modified package config from the system's DB.
copyFromDB :: FilePath -> String -> IO ()
copyFromDB hastepkg package = do
  (_, pkgdata, _, ghcpkgProc) <- runInteractiveProcess "ghc-pkg"
                                                       args
                                                       Nothing
                                                       Nothing
  hastepkgDone <- newEmptyMVar
  _ <- forkIO $ savePkgConfig hastepkg pkgdata hastepkgDone 
  _ <- waitForProcess ghcpkgProc
  takeMVar hastepkgDone
  return ()
  where
    args = ["describe", package]

-- | Modify and save a config read from a handle.
savePkgConfig :: FilePath -> Handle -> MVar () -> IO ()
savePkgConfig hastepkg pkgdata hastepkgDone = do
  (indata, _, _, hpkgProc) <- runInteractiveProcess hastepkg
                                                    ["update", "-", "--force"]
                                                    Nothing
                                                    Nothing
  pkgtext <- hGetContents pkgdata
  hPutStrLn indata (fixPaths pkgtext)
  hClose indata
  _ <- waitForProcess hpkgProc
  putMVar hastepkgDone ()

-- | Hack a config to work with Haste.
fixPaths :: String -> String
fixPaths pkgtext =
  pkgtext'
  where
    pkgtext' = unlines
             . map fixPath
             . filter (not . ("haddock" `isPrefixOf`))
             . filter (not . ("hs-libraries:" `isPrefixOf`))
             $ lines pkgtext
    
    fixPath str
      | isKey "library-dirs:" str =
        "library-dirs: " ++ importDir </> pkgdir
{-      | isKey "include-dirs:" str =
        "include-dirs: " ++ importDir </> pkgdir -}
      | isKey "import-dirs:" str =
        "import-dirs: " ++ importDir </> pkgdir
      | isKey "pkgroot:" str =
        "pkgroot: \"" ++ pkgRoot ++ "\""
      | "-inplace" `isSuffixOf` str =
        reverse $ drop (length "-inplace") $ reverse str
      | otherwise =
        str
      where
        pkgdir =
          case reverse str of
            (':':_) -> ""
            str'    -> reverse $ takeWhile (/= '/') str'

    isKey _ "" =
      False
    isKey key str =
      and $ zipWith (==) key str
    
    importDir = hasteDir </> "haste-install" </> "lib"
    pkgRoot   = hasteDir </> "haste-install"
