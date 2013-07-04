{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Haste.Linker (link) where
import Haste.Config
import Haste.Module
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Applicative
import Module hiding (Module)
import Data.JSTarget
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Builder
import Data.Monoid

-- | File extension for files housing JSMods.
modExt :: String
modExt = ".jsmod"

-- | The program entry point.
mainSym :: Name
mainSym = name "main" (Just "Main")

-- | Link a program using the given config and input file name.
link :: Config -> FilePath -> IO ()
link cfg target = do
  defs <- getAllDefs (libPath cfg) mainSym
  let myDefs = if wholeProgramOpts cfg then topLevelInline defs else defs
  let (progText, mainSym') = prettyProg (ppOpts cfg) mainSym myDefs
      callMain = appStart cfg mainSym'
  
  rtslibs <- mapM readFile $ rtsLibs cfg ++ jsExternals cfg
  B.writeFile (outFile cfg target)
    $ toLazyByteString 
    $ stringUtf8 (unlines rtslibs)
    <> progText
    <> callMain

-- | Generate a sequence of all assignments needed to run Main.main.
getAllDefs :: FilePath -> Name -> IO (AST Stm)
getAllDefs libpath mainsym =
  runDep $ addDef libpath mainsym

data DepState = DepState {
    defs        :: !(AST Stm -> AST Stm),
    alreadySeen :: !(S.Set Name),
    modules     :: !(M.Map String Module)
  }

newtype DepM a = DepM (StateT DepState IO a)
  deriving (Monad, MonadIO)

initState :: DepState
initState = DepState {
    defs        = id,
    alreadySeen = S.empty,
    modules     = M.empty
  }

-- | Run a dependency resolution computation.
runDep :: DepM a -> IO (AST Stm)
runDep (DepM m) = do
  defs' <- defs . snd <$> runStateT m initState
  return (defs' nullRet)

instance MonadState DepState DepM where
  get = DepM $ get
  put = DepM . put

-- | Return the module the given variable resides in.
getModuleOf :: FilePath -> Name -> DepM Module
getModuleOf libpath v =
  case moduleOf v of
    Just "GHC.Prim" -> return foreignModule
    Just ""         -> return foreignModule
    Just m          -> getModule libpath
                       $ (++ modExt)
                       $ moduleNameSlashes
                       $ mkModuleName m
    _               -> return foreignModule

-- | Return the module at the given path, loading it into cache if it's not
--   already there.
getModule :: FilePath -> FilePath -> DepM Module
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
addDef :: FilePath -> Name -> DepM ()
addDef libpath v = do
  st <- get
  when (not $ v `S.member` alreadySeen st) $ do
    m <- getModuleOf libpath v

    -- getModuleOf may update the state, so we need to refresh it
    st' <- get
    let dependencies = maybe S.empty id (M.lookup v (modDeps m))
    put st' {alreadySeen = S.insert v (alreadySeen st')}
    S.foldl' (\a x -> a >> addDef libpath x) (return ()) dependencies

    -- addDef _definitely_ updates the state, so refresh once again
    st'' <- get
    let  Name comment _ = v
         defs' =
           maybe (defs st'')
                 (\body -> defs st'' . newVar True (internalVar v comment) body)
                 (M.lookup v (modDefs m))
    put st'' {defs = defs'}
