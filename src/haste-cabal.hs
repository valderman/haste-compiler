-- | haste-cabal - Haste wrapper for cabal.
module Main where
import System.Environment
import System.Exit
import Haste.Environment
import Control.Shell
import Control.Monad (when)
import Data.List

type Match = (String -> Bool, [String] -> [String])

cabal :: [String] -> IO ()
cabal args = do
  res <- shell $ run_ "cabal" (hasteargs ++ args') ""
  case res of
    Left _ -> exitFailure
    _      -> exitSuccess
  where
    args' = [arg | arg <- args, arg /= "--install-global", arg /= "--global"]
    hasteargs
      | "update" `elem` args =
        []
      | "build" `elem` args =
        ["--with-ghc=" ++ hasteBinary]
      | otherwise =
        ["--with-compiler=" ++ hasteBinary,
         "--with-hc-pkg=" ++ hastePkgBinary,
         "--with-hsc2hs=hsc2hs",
         "-fhaste-cabal"] ++
        if "--install-global" `elem` args || "--global" `elem` args
           then ["--prefix=" ++ hasteCabalSysDir,
                 "--package-db=" ++ pkgSysDir]
           else ["--prefix=" ++ hasteCabalUserDir,
                 "--package-db=" ++ pkgSysDir,
                 "--package-db=" ++ pkgUserDir]


main :: IO ()
main = do
  as <- getArgs
  when (hasteCabalNeedsReboot && not ("--unbooted" `elem` as)) $ do
    putStrLn "WARNING: haste-cabal has not been properly booted."
    putStrLn "If you experience problems installing packages, or simply want to"
    putStrLn "get rid of this message, please run 'haste-boot'."

  if "update" `elem` as
    then do
      cabal as
    else do
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
