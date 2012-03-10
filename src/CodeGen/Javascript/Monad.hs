{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CodeGen.Javascript.Monad (JSGen, genJS, emit, merge) where
import Control.Monad.State
import CodeGen.Javascript.Bag as Bag
import CodeGen.Javascript.AST hiding (code)

newtype GenState = GenState {
    code :: Bag JSStmt
  }

initialState :: GenState
initialState = GenState {
    code = empty
  }

newtype JSGen a = JSGen {unJSG :: State GenState a} deriving Monad

genJS :: JSGen a -> (a, Bag JSStmt)
genJS (JSGen gen) = case runState gen initialState of
  (a, st) -> (a, code st)

-- | Emit a JS statement to the code stream
emit :: JSStmt -> JSGen ()
emit stmt = JSGen $ do
  st <- get
  put st {code = code st `snoc` stmt}

-- | Emit an entire chain of JS statements
merge :: Bag JSStmt -> JSGen ()
merge stmts = JSGen $ do
  st <- get
  put st {code = code st `Bag.concat` stmts}
