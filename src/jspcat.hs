-- | jspcat - print the contents of a JSMod intermediate file.
module Main where
import System.Environment (getArgs)
import Haste
import Haste.AST (bogusJSVar)

main = do
  as <- getArgs
  if null as
     then putStrLn "Usage: jspcat Mod1.jsmod [Mod2.jsmod, ...]"
     else mapM_ catMod as
  where
    catMod m =
      readModule "." m >>= putStrLn . fst . prettyJS pseudo bogusJSVar
