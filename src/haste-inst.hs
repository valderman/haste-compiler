-- | haste-inst - Haste wrapper for cabal.
module Main where
import System.FilePath
import System.Environment
import EnvUtils

cabal :: [String] -> IO ()
cabal args = do
  runAndWait "cabal" (hasteargs ++ args) Nothing
  where
    hasteargs 
      | "build" `elem` args =
        ["--with-ghc=" ++ hasteBinary]
      | otherwise =
        ["--with-compiler=" ++ hasteBinary,
         "--with-hc-pkg=haste-pkg",
         "--prefix=" ++ hasteDir </> "haste-install"]

main :: IO ()
main = do
  as <- getArgs
  cabal (hasteargs as)
  where
    libinstall = "--ghc-options=\"--libinstall\""
    hasteargs as
    -- If the user passes --install-jsmods, hastec is invoked with --libinstall
    -- even if running haste-inst build.
      | "--install-jsmods" `elem` as =
        libinstall : filter (/= "--install-jsmods") as
      | "build" `elem` as =
        as
      | otherwise =
        libinstall : as
