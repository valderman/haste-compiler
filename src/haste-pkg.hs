-- | haste-pkg; wrapper for ghc-pkg.
module Main where
import System.Environment
import System.Process

main = do
  args <- getArgs
  rawSystem "ghc-pkg" args
