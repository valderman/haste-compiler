module Main where
import System.Environment
import Haste.Module
import Haste.Config
import Data.JSTarget
import Data.JSTarget.PP
import Data.Maybe
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as BS

main = do
  as <- getArgs
  mapM_ printModule as

printModule mpkg = do
  let (pkg, (_:m)) = break (== ':') mpkg
  mods <- mapM (\p -> readModule p pkg m) ("." : libPaths def)
  mapM_ printDefs . map fromJust $ filter isJust mods

printDefs mod = do
  mapM_ printDef $ M.toList $ modDefs mod

printDef (name, def) = do
  putStrLn $ niceName name
  BS.putStrLn $ pretty debugPPOpts def
  putStrLn ""

niceName (Name n (Just (pkg, m))) =
  pkg ++ ":" ++ m ++ "." ++ n
niceName (Name n _) =
  n
