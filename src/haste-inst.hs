-- | haste-inst - Haste wrapper for cabal.
module Main where
import System.FilePath
import System.Environment
import EnvUtils
import Data.List

type Match = (String -> Bool, [String] -> [String])

cabal :: [String] -> IO ()
cabal args = do
  runAndWait "cabal" (hasteargs ++ args) Nothing
  where
    hasteargs 
      | "build" `elem` args =
        ["--with-ghc=" ++ hasteBinary]
      | otherwise =
        ["--with-compiler=" ++ hasteBinary,
         "--with-hc-pkg=" ++ hastePkgBinary,
         "--with-hsc2hs=hsc2hs",
         "--prefix=" ++ hasteDir </> "haste-install",
         "--package-db=" ++ pkgDir]

main :: IO ()
main = do
  as <- getArgs
  as <- return $ if "--install-jsmods" `elem` as || not ("build" `elem` as)
                   then libinstall : filter (/= "--install-jsmods") as
                   else as
  as <- return $ if "--unbooted" `elem` as
                   then unbooted : filter (/= "--unbooted") as
                   else as
  cabal as
  where
    libinstall = "--ghc-option=--libinstall"
    unbooted   = "--ghc-option=--unbooted"
