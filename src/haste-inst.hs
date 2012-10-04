-- | haste-inst - Haste wrapper for cabal.
module Main where
import System.FilePath
import System.Environment
import EnvUtils

cabal :: [String] -> IO ()
cabal args = do
  runAndWait "cabal" (hasteargs ++ args) Nothing
  where
    hasteargs = ["--with-compiler=" ++ hasteBinary,
                 "--with-hc-pkg=haste-pkg",
                 "--prefix=" ++ hasteDir </> "haste-install",
                 "--ghc-options=\"--libinstall\""]

main :: IO ()
main = getArgs >>= cabal
