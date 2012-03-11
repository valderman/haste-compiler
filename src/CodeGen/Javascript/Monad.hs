{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module CodeGen.Javascript.Monad (JSGen, genJS, emit, dependOn) where
import Control.Monad.State
import Bag
import CodeGen.Javascript.AST hiding (code, deps)
import qualified Data.Set as S

data GenState = GenState {
    code :: !(Bag JSStmt),
    deps :: !(S.Set JSVar)
  }

initialState :: GenState
initialState = GenState {
    code = emptyBag,
    deps = S.empty
  }

newtype JSGen a = JSGen (State GenState a) deriving Monad

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

genJS :: JSGen a -> (a, S.Set JSVar, Bag JSStmt)
genJS (JSGen gen) = case runState gen initialState of
  (a, GenState stmts dependencies) -> (a, dependencies, stmts)

-- | Emit a JS statement to the code stream
emit :: JSStmt -> JSGen ()
emit stmt = JSGen $ do
  st <- get
  put st {code = code st `snocBag` stmt}
