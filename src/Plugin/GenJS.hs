module Plugin.GenJS (plugin) where
import GhcPlugins
import CodeGen.Javascript

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ passes = do
  reinitializeGlobals
  return $ passes ++ [genJS]

desc = "Generate Javascript code as a side effect"

genJS :: CoreToDo
genJS = CoreDoPluginPass desc $ \x -> do
  liftIO . writeModule $ generate x
  return x
