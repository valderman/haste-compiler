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

import Debug.Trace

-- | File extension for files housing JSMods.
moduleExtension :: String
moduleExtension = ".jsmod"

main = do
  putStrLn =<< prettyJS pretty . bagToList <$> getAllDefs

-- | Generate a Bag of all functions needed to run Main.main.
getAllDefs :: IO (Bag JSStmt)
getAllDefs = do
  runDep $ addDef $ (JSVar "Main" $ External "main")

data DepState = DepState {
    defs        :: Bag JSStmt,
    alreadySeen :: S.Set JSVar,
    modules     :: M.Map String JSMod
  }

newtype DepM a = DepM (StateT DepState IO a)
  deriving (Monad, MonadIO)

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
    "" -> return foreignModule
    m  -> getModule $ (++ moduleExtension) $ moduleNameSlashes $ mkModuleName m

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
    S.foldl' (\a x -> a >> addDef x) (return ()) dependencies

    st <- get    
    let defs' = maybe (defs st)
                      (\body -> defs st `snocBag` NewVar (Var var) body)
                      (M.lookup var (code m))

    put st {defs        = defs',
            alreadySeen = S.insert var (alreadySeen st)}
