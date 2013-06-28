{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Haste.Monad (
  JSGen, genJS, emit, dependOn, getModName, pushBinding, popBinding,
  getCurrentBinding, getCurrentBindingArgs, isolate, addLocal, getCfg) where
import Control.Monad.State
import Bag
import Haste.AST hiding (code, deps)
import qualified Data.Set as S
import Control.Applicative
import GhcPlugins (Var)

data GenState cfg = GenState {
    code         :: !(Bag JSStmt),
    deps         :: !(S.Set JSVar),
    locals       :: !(S.Set JSVar),
    modName      :: JSLabel,
    bindingStack :: [(Var, [Var])],
    config       :: cfg
  }

initialState :: GenState cfg
initialState = GenState {
    code         = emptyBag,
    deps         = S.empty,
    locals       = S.empty,
    modName      = undefined,
    bindingStack = [],
    config       = undefined
  }

newtype JSGen cfg a =
  JSGen (State (GenState cfg) a)
  deriving (Monad, Functor, Applicative)

class Dependency a where
  -- | Add a dependency to the function currently being generated.
  dependOn :: a -> JSGen cfg ()
  -- | Mark a symbol as local, excluding it from the dependency graph.
  addLocal :: a -> JSGen cfg ()

instance Dependency JSVar where
  dependOn var = JSGen $ do
    st <- get
    put st {deps = S.insert var (deps st)}

  addLocal var = JSGen $ do
    st <- get
    put st {locals = S.insert var (locals st)}

instance Dependency (S.Set JSVar) where
  dependOn vars = JSGen $ do
    st <- get
    put st {deps = S.union vars (deps st)}

  addLocal vars = JSGen $ do
    st <- get
    put st {locals = S.union vars (locals st)}

instance Dependency [JSVar] where
  dependOn vars = JSGen $ do
    st <- get
    put st {deps = S.union (S.fromList vars) (deps st)}

  addLocal vars = JSGen $ do
    st <- get
    put st {locals = S.union (S.fromList vars) (locals st)}

genJS :: cfg         -- ^ Config to use for code generation.
      -> JSLabel     -- ^ Name of the module being compiled.
      -> JSGen cfg a -- ^ The code generation computation.
      -> (a, S.Set JSVar, S.Set JSVar, Bag JSStmt)
genJS cfg myModName (JSGen gen) =
  case runState gen initialState {modName = myModName, config = cfg} of
    (a, GenState stmts dependencies loc _ _ _) ->
      (a, dependencies, loc, stmts)

-- | Emit a JS statement to the code stream
emit :: JSStmt -> JSGen cfg ()
emit stmt = JSGen $ do
  st <- get
  put st {code = code st `snocBag` stmt}

getModName :: JSGen cfg JSLabel
getModName = JSGen $ modName <$> get

-- | Get the Var for the binding currently being generated.
getCurrentBinding :: JSGen cfg Var
getCurrentBinding = JSGen $ (fst . head . bindingStack) <$> get

-- | Get the Var for the args of the binding currently being generated.
getCurrentBindingArgs :: JSGen cfg [Var]
getCurrentBindingArgs = JSGen $ (snd . head . bindingStack) <$> get

-- | Push a new var onto the stack, indicating that we're generating code
--   for a new binding.
pushBinding :: Var -> [Var] -> JSGen cfg ()
pushBinding var args = JSGen $ do
  st <- get
  put st {bindingStack = (var, args) : bindingStack st}

-- | Pop a var from the stack, indicating that we're done generating code
--   for that binding.
popBinding :: JSGen cfg ()
popBinding = JSGen $ do
  st <- get
  put st {bindingStack = tail $ bindingStack st}

-- | Run a GenJS computation in isolation, returning its results rather than
--   writing them to the output stream. Dependencies and locals are still
--   updated, however.
isolate :: JSGen cfg a -> JSGen cfg (a, Bag JSStmt)
isolate gen = do
  myMod <- getModName
  myBnd <- getCurrentBinding
  myArgs <- getCurrentBindingArgs
  cfg <- getCfg
  let (x, dep, loc, stmts) = genJS cfg myMod $ do
        pushBinding myBnd myArgs >> gen
  dependOn dep
  addLocal loc
  return (x, stmts)

getCfg :: JSGen cfg cfg
getCfg = JSGen $ fmap config get
