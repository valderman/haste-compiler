{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module CodeGen.Javascript.Monad (JSGen, genJS, emit, dependOn, getModName) where
import Control.Monad.State
import Bag
import CodeGen.Javascript.AST hiding (code, deps)
import qualified Data.Set as S
import Control.Applicative

data GenState = GenState {
    code    :: !(Bag JSStmt),
    deps    :: !(S.Set JSVar),
    modName :: JSLabel
  }

initialState :: GenState
initialState = GenState {
    code    = emptyBag,
    deps    = S.empty,
    modName = undefined
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
    (a, GenState stmts dependencies _) -> (a, dependencies, stmts)

-- | Emit a JS statement to the code stream
emit :: JSStmt -> JSGen ()
emit stmt = JSGen $ do
  st <- get
  put st {code = code st `snocBag` stmt}

getModName :: JSGen JSLabel
getModName = JSGen $ modName `fmap` get