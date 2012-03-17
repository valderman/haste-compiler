{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
-- | jsplink - link together a set of modules to form a JS blob.
module Main where
import System.Environment (getArgs)
import CodeGen.Javascript
import CodeGen.Javascript.AST
import Bag
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Applicative
import Module
import Paths_jsplug

-- | File extension for files housing JSMods.
modExt :: String
modExt = ".jsmod"


main = do
  args <- getArgs
  let debugOutput = any (== "debug") args
      printMode   = if debugOutput then pretty else compact

  debug <- if debugOutput
              then getDataFileName "debug.js" >>= readFile
              else return ""
  rts <- getDataFileName "rts.js" >>= readFile
  
  defs <- bagToList <$> getAllDefs mainSym
  let (progText, mainSym') = prettyJS printMode mainSym defs
      callMain = mainSym' ++ "(0);"
  
  putStrLn $ unlines [
    rts,
    debug,
    progText,
    callMain]
