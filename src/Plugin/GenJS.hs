module Plugin.GenJS (plugin) where
import GhcPlugins
import TidyPgm
import CodeGen.Javascript
import System.Directory
import System.FilePath (combine)
import Control.Applicative

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts passes = do
  reinitializeGlobals
  return $ passes ++ [genJS opts]

desc :: String
desc = "Generate Javascript code as a side effect"

genJS :: [CommandLineOption] -> CoreToDo
genJS opts = CoreDoPluginPass desc $ \mguts -> do
  env <- getHscEnv
  targetdir <- getTargetDir opts
  (cgguts, _) <- liftIO $ tidyProgram env mguts
  liftIO . writeModule targetdir $ generate cgguts
  return mguts

getTargetDir :: [CommandLineOption] -> CoreM FilePath
getTargetDir xs
  | any (== "libinstall") xs =
    append "lib" <$> liftIO (getAppUserDataDirectory "jsplug")
  | otherwise =
    return "."

append :: FilePath -> FilePath -> FilePath
append = flip combine