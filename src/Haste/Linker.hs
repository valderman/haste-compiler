{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Haste.Linker (link) where
import Haste.Config
import Haste.Module
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Applicative
import Data.JSTarget
import qualified Data.ByteString.Lazy as B
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Data.Monoid

-- | The program entry point.
--   This will need to change when we start supporting building "binaries"
--   using cabal, since we'll have all sorts of funny package names then.
mainSym :: Name
mainSym = name "main" (Just ("main", "Main"))

-- | Link a program using the given config and input file name.
link :: Config -> String -> FilePath -> IO ()
link cfg pkgid target = do
  ds <- getAllDefs (libPath cfg) pkgid mainSym
  let myDefs = if wholeProgramOpts cfg then topLevelInline ds else ds
  let (progText, mainSym') = prettyProg (ppOpts cfg) mainSym myDefs
      callMain = fromString "A(" <> mainSym' <> fromString ", [0]);"
      launchApp = appStart cfg (fromString "hasteMain")
  
  rtslibs <- mapM readFile $ rtsLibs cfg
  extlibs <- mapM readFile $ jsExternals cfg
  B.writeFile (outFile cfg cfg target)
    $ toLazyByteString
    $ assembleProg (wrapProg cfg) extlibs rtslibs progText callMain launchApp
  where
    assembleProg True extlibs rtslibs progText callMain launchApp =
      fromString (unlines extlibs)
      <> fromString "var hasteMain = function() {"
      <> fromString (unlines rtslibs)
      <> progText
      <> callMain
      <> fromString "};\n"
      <> launchApp
    assembleProg _ extlibs rtslibs progText callMain launchApp =
      fromString (unlines extlibs)
      <> fromString (unlines rtslibs)
      <> progText
      <> fromString "\nvar hasteMain = function() {" <> callMain
                                                     <> fromString "};"
      <> launchApp


-- | Generate a sequence of all assignments needed to run Main.main.
getAllDefs :: FilePath -> String -> Name -> IO (AST Stm)
getAllDefs libpath pkgid mainsym =
  runDep $ addDef libpath pkgid mainsym

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
    Just m          -> getModule libpath (maybe "main" id $ pkgOf v) m
    _               -> return foreignModule

-- | Return the module at the given path, loading it into cache if it's not
--   already there.
getModule :: FilePath -> String -> String -> DepM Module
getModule libpath pkgid modname = do
  st <- get
  case M.lookup modname (modules st) of
    Just m ->
      return m
    _      -> do
      liftIO $ putStrLn $ "Linking " ++ modname
      m <- liftIO $ readModule libpath pkgid modname
      put st {modules = M.insert modname m (modules st)}
      return m

-- | Add a new definition and its dependencies. If the given identifier has
--   already been added, it's just ignored.
addDef :: FilePath -> String -> Name -> DepM ()
addDef libpath pkgid v = do
  st <- get
  when (not $ v `S.member` alreadySeen st) $ do
    m <- getModuleOf libpath v

    -- getModuleOf may update the state, so we need to refresh it
    st' <- get
    let dependencies = maybe S.empty id (M.lookup v (modDeps m))
    put st' {alreadySeen = S.insert v (alreadySeen st')}
    S.foldl' (\a x -> a >> addDef libpath pkgid x) (return ()) dependencies

    -- addDef _definitely_ updates the state, so refresh once again
    st'' <- get
    let  Name comment _ = v
         defs' =
           maybe (defs st'')
                 (\body -> defs st'' . newVar True (internalVar v comment) body)
                 (M.lookup v (modDefs m))
    put st'' {defs = defs'}
