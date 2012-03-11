-- | jsplink - link together a set of modules to form a JS blob.
module Main where
import System.Environment (getArgs)
import CodeGen.Javascript

main = do
  as <- getArgs
  if null as
     then putStrLn "Usage: jsplink Mod1.jsmod [Mod2.jsmod, ...]"
     else mapM_ catMod as
  where
    catMod m =
      readModule m >>= return . prettyJS compact >>= putStrLn
