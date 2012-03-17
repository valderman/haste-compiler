{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module CodeGen.Javascript.Linker (link) where
import CodeGen.Javascript.Config
import CodeGen.Javascript.AST
import CodeGen.Javascript.PrintJS
import CodeGen.Javascript.Module
import Bag
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Applicative
import Module

-- | File extension for files housing JSMods.
modExt :: String
modExt = ".jsmod"

-- | The program entry point.
mainSym :: JSVar
mainSym = JSVar {
    jsmod  = "Main",
    jsname = External "main"
  }


-- | Link a program using the given config and input file name.
link :: Config -> FilePath -> IO ()
link cfg target = do
  myDefs <- bagToList <$> getAllDefs mainSym
  let (progText, mainSym') = prettyJS (ppOpts cfg) mainSym myDefs
      callMain = appStart cfg mainSym'
  
  rtslibs <- mapM readFile $ rtsLibs cfg
  writeFile (outFile cfg target)
    $ unlines
    $ rtslibs ++ [progText, callMain]


-- | Generate a Bag of all functions needed to run Main.main.
getAllDefs :: JSVar -> IO (Bag JSStmt)
getAllDefs mainsym =
  runDep $ addDef mainsym

data DepState = DepState {
    defs        :: Bag JSStmt,
    alreadySeen :: S.Set JSVar,
    modules     :: M.Map String JSMod
  }

newtype DepM a = DepM (StateT DepState IO a)
  deriving (Monad, MonadIO)

initState :: DepState
initState = DepState {
    defs        = emptyBag,
    alreadySeen = S.empty,
    modules     = M.empty
  }

-- | Run a dependency resolution computation.
runDep :: DepM a -> IO (Bag JSStmt)
runDep (DepM m) = defs . snd <$> runStateT m initState

instance MonadState DepState DepM where
  get = DepM $ get
  put = DepM . put

-- | Return the module the given variable resides in.
getModuleOf :: JSVar -> DepM JSMod
getModuleOf var =
  case jsmod var of
    ""    -> return foreignModule
    m     -> getModule $ (++ modExt) $ moduleNameSlashes $ mkModuleName m

-- | Return the module at the given path, loading it into cache if it's not
--   already there.
getModule :: FilePath -> DepM JSMod
getModule path = do
  st <- get
  case M.lookup path (modules st) of
    Just m ->
      return m
    _      -> do
      m <- liftIO $ readModule "." path
      put st {modules = M.insert path m (modules st)}
      return m

-- | Add a new definition and its dependencies. If the given identifier has
--   already been added, it's just ignored.
addDef :: JSVar -> DepM ()
addDef var = do
  st <- get
  when (not $ var `S.member` alreadySeen st) $ do
    m <- getModuleOf var
    let dependencies = maybe S.empty id (M.lookup var (deps m))
    put st {alreadySeen = S.insert var (alreadySeen st)}
    S.foldl' (\a x -> a >> addDef x) (return ()) dependencies

    st' <- get    
    let defs' = maybe (defs st')
                      (\body -> defs st' `snocBag` NewVar (Var var) body)
                      (M.lookup var (code m))
    put st' {defs = defs'}
