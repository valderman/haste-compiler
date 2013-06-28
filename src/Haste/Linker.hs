{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Haste.Linker (link) where
import Haste.Config
import Haste.AST
import Haste.PrintJS
import Haste.Module
import Bag
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State.Strict
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
  myDefs <- bagToList <$> getAllDefs (libPath cfg) mainSym
  let (progText, mainSym') = prettyJS (ppOpts cfg) mainSym myDefs
      callMain = appStart cfg mainSym'
  
  rtslibs <- mapM readFile $ rtsLibs cfg ++ jsExternals cfg
  writeFile (outFile cfg target)
    $ unlines
    $ rtslibs ++ [progText, callMain]


-- | Generate a Bag of all functions needed to run Main.main.
getAllDefs :: FilePath -> JSVar -> IO (Bag JSStmt)
getAllDefs libpath mainsym =
  runDep $ addDef libpath mainsym

data DepState = DepState {
    defs        :: !(Bag JSStmt),
    alreadySeen :: !(S.Set JSVar),
    modules     :: !(M.Map String JSMod)
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
getModuleOf :: FilePath -> JSVar -> DepM JSMod
getModuleOf libpath var =
  case jsmod var of
    "GHC.Prim" -> return foreignModule
    ""         -> return foreignModule
    m          -> getModule libpath
                    $ (++ modExt)
                    $ moduleNameSlashes
                    $ mkModuleName m

-- | Return the module at the given path, loading it into cache if it's not
--   already there.
getModule :: FilePath -> FilePath -> DepM JSMod
getModule libpath modpath = do
  st <- get
  case M.lookup modpath (modules st) of
    Just m ->
      return m
    _      -> do
      liftIO $ putStrLn $ "Linking " ++ modpath
      m <- liftIO $ readModule libpath modpath
      put st {modules = M.insert modpath m (modules st)}
      return m

-- | Add a new definition and its dependencies. If the given identifier has
--   already been added, it's just ignored.
addDef :: FilePath -> JSVar -> DepM ()
addDef libpath var = do
  st <- get
  when (not $ var `S.member` alreadySeen st) $ do
    m <- getModuleOf libpath var

    -- getModuleOf may update the state, so we need to refresh it
    st' <- get
    let dependencies = maybe S.empty id (M.lookup var (deps m))
    put st' {alreadySeen = S.insert var (alreadySeen st')}
    S.foldl' (\a x -> a >> addDef libpath x) (return ()) dependencies

    -- addDef _definitely_ updates the state, so refresh once again
    st'' <- get    
    let defs' = maybe (defs st'')
                      (\body -> defs st'' `snocBag` NewVar (Var var) body)
                      (M.lookup var (code m))
    put st'' {defs = defs'}
