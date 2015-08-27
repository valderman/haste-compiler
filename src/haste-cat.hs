{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Main where
import System.Environment
import Haste.Module
import Haste.Config
import Data.JSTarget
import Data.JSTarget.PP
import Data.Maybe
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS

main = do
  as <- getArgs
  if null as
    then putStrLn "Usage: haste-cat package-id:Module.To.Inspect"
    else mapM_ printModule as

printModule mpkg = do
  let (pkg, (_:mn)) = break (== ':') mpkg
      paths = "." : libPaths def
  mods <- mapM (\p -> (p, ) `fmap` readModule p pkg mn) paths
  case filter (isJust . snd) mods of
    ((p, Just m):_) -> printDefs p pkg mn m
    _               -> return ()

printDefs :: FilePath -> String -> String -> Module -> IO ()
printDefs path pkg mn m = do
  putStrLn $ "Package: " ++ pkg
  putStrLn $ "Module:  " ++ mn
  putStrLn $ "Path:    " ++ path
  putStrLn "---\n"
  mapM_ printDef $ M.toList $ modDefs m

printDef (name, d) = do
  BS.putStrLn $ niceName name
  BSL.putStrLn $ pretty (withPretty . withExtAnnotation $ withAnnotations def) d
  putStrLn ""

niceName (Name n (Just (pkg, m))) =
  BS.concat [pkg, ":", m, ".", n]
niceName (Name n _) =
  n
