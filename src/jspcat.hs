-- | jspcat - print the contents of a JSMod intermediate file.
module Main where
import System.Environment (getArgs)
import CodeGen.Javascript

main = do
  as <- getArgs
  if null as
     then putStrLn "Usage: jspcat Mod1.jsmod [Mod2.jsmod, ...]"
     else mapM_ catMod as
  where
    catMod m =
      readModule m >>= return . prettyJS pseudo >>= putStrLn
