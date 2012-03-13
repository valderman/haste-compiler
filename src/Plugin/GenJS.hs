module Plugin.GenJS (plugin) where
import GhcPlugins
import TidyPgm
import GHC
import CodeGen.Javascript

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ passes = do
  reinitializeGlobals
  return $ passes ++ [genJS]

desc :: String
desc = "Generate Javascript code as a side effect"

genJS :: CoreToDo
genJS = CoreDoPluginPass desc $ \mguts -> do
  env <- getHscEnv
  (cgguts, _) <- liftIO $ tidyProgram env mguts
  liftIO . writeModule $ generate cgguts
  return mguts
