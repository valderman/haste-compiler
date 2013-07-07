-- | haste-copy-pkg; copy a package from file or GHC package DB and fix up its
--   paths.
module Main where
import Data.List
import Haste.Environment
import System.Environment (getArgs)
import Control.Shell

main :: IO ()
main = do
  args <- getArgs
  let (pkgdbs, pkgs) = partition ("--package-db=" `isPrefixOf`) args
  if null args
    then do
      putStrLn "Usage: haste-copy-pkg [--package-db=foo.conf] <packages>"
    else do
      res <- shell (mapM_ (copyFromDB pkgdbs) pkgs)
      case res of
        Left err -> error err
        _        -> return ()

copyFromDB :: [String] -> String -> Shell ()
copyFromDB pkgdbs package = do
  pkgdesc <- run "ghc-pkg" (["describe", package] ++ pkgdbs) ""
  run_ "haste-pkg" ["update", "-", "--force"] (fixPaths package pkgdesc)

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
    
    importDir = pkgLibDir
    pkgRoot   = hasteInstDir
