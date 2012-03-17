{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module CodeGen.Javascript.Monad (
  JSGen, genJS, emit, dependOn, getModName, pushBinding, popBinding,
  getCurrentBinding, isolate) where
import Control.Monad.State
import Bag
import CodeGen.Javascript.AST hiding (code, deps)
import qualified Data.Set as S
import Control.Applicative
import GhcPlugins (Var)

data GenState = GenState {
    code         :: !(Bag JSStmt),
    deps         :: !(S.Set JSVar),
    modName      :: JSLabel,
    bindingStack :: [Var]
  }

initialState :: GenState
initialState = GenState {
    code         = emptyBag,
    deps         = S.empty,
    modName      = undefined,
    bindingStack = []
  }

newtype JSGen a =
  JSGen (State GenState a)
  deriving (Monad, Functor, Applicative)

class Dependency a where
  -- | Add a dependency to the function currently being generated.
  dependOn :: a -> JSGen ()

instance Dependency JSVar where
  dependOn var = JSGen $ do
    st <- get
    put st {deps = S.insert var (deps st)}

instance Dependency (S.Set JSVar) where
  dependOn vars = JSGen $ do
    st <- get
    put st {deps = S.union vars (deps st)}

genJS :: JSLabel -> JSGen a -> (a, S.Set JSVar, Bag JSStmt)
genJS myModName (JSGen gen) =
  case runState gen initialState {modName = myModName} of
    (a, GenState stmts dependencies _ _) -> (a, dependencies, stmts)

-- | Emit a JS statement to the code stream
emit :: JSStmt -> JSGen ()
emit stmt = JSGen $ do
  st <- get
  put st {code = code st `snocBag` stmt}

getModName :: JSGen JSLabel
getModName = JSGen $ modName <$> get

-- | Get the Var for the binding currently being generated.
getCurrentBinding :: JSGen Var
getCurrentBinding = JSGen $ (head . bindingStack) <$> get

-- | Push a new var onto the stack, indicating that we're generating code
--   for a new binding.
pushBinding :: Var -> JSGen ()
pushBinding var = JSGen $ do
  st <- get
  put st {bindingStack = var : bindingStack st}

-- | Pop a var from the stack, indicating that we're done generating code
--   for that binding.
popBinding :: JSGen ()
popBinding = JSGen $ do
  st <- get
  put st {bindingStack = tail $ bindingStack st}

-- | Run a GenJS computation in isolation, returning its results rather than
--   writing them to the output stream.
isolate :: JSGen a -> JSGen (a, S.Set JSVar, Bag JSStmt)
isolate gen = do
  myMod <- getModName
  myBnd <- getCurrentBinding
  return $ genJS myMod $ do
    pushBinding myBnd >> gen
