-- | jsplink - link together a set of modules to form a JS blob.
module Main where
import System.Environment (getArgs)
import CodeGen.Javascript
import CodeGen.Javascript.AST
import Bag
import qualified Data.Map as M
import qualified Data.Set as S

main = do
  as <- getArgs
  if null as
     then putStrLn "Usage: jsplink Main.jsmod"
     else addMain (head as) >>= putStrLn . prettyJS pretty . bagToList

addMain :: FilePath -> IO (Bag JSStmt)
addMain path = do
  mod <- readModule path
  let mainId = External "Main.main"
  case M.lookup mainId (code mod) of
    Just mainFun -> do
      return $ addFun emptyBag mod (NewVar (Var mainId) mainFun)
    _ ->
      fail "Module doesn't have a main!"

addFun :: Bag JSStmt -> JSMod -> JSStmt -> Bag JSStmt
addFun ds mod fun@(NewVar (Var funName) body) =
  (myDeps `snocBag` fun) `unionBags` ds
  where
    myDeps =
      case M.lookup funName (deps mod) of
        Just depSet -> depsFromSet depSet
        _           -> emptyBag

    depsFromSet :: S.Set JSVar -> Bag JSStmt
    depsFromSet s =
      S.foldl' addFromSet emptyBag s
    
    addFromSet acc fn =
      case M.lookup fn (code mod) of
        Just body -> addFun acc mod (NewVar (Var fn) body)
        _         -> acc
