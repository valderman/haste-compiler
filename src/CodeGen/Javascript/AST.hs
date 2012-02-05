{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Abstract syntax for the Javascript constructs needed to generate JS from
--   Core.
module CodeGen.Javascript.AST (
  JSVar (..), VarID, varZero, JSStmt (..), JSAlt (..), JSExp (..), JSLit (..),
  JSOp (..)) where

data JSVar
  = Arg VarID          -- ^ Function argument; may or may not be a thunk
  | Strict VarID       -- ^ A local temporary value; is definitely not a thunk
  | Lazy VarID         -- ^ A local temporary thunk; obviously always a thunk
  | NamedLazy String   -- ^ A named lazy variable, probably top level
  | NamedStrict String -- ^ A named strict variable, also probably top level
    deriving (Show)

newtype VarID = VarID Int deriving (Show, Enum)

varZero :: VarID
varZero = VarID 0

data JSStmt
  = Ret JSExp
  | CallRet JSExp [JSExp] -- Unused; use for CSE
  | While JSExp JSStmt -- Unused; use for CSE
  | Block [JSStmt]
  | Case JSExp [JSAlt]
  | Assign JSExp JSExp
  | NamedFun String [JSVar] [JSStmt] -- Unused; turn top level defs into tihs
    deriving Show

data JSAlt
  = Cond JSExp  [JSStmt]
  | Def         [JSStmt]
  | Cons String [JSStmt]
    deriving Show

data JSExp
  = Call JSExp [JSExp]
  | Fun [JSVar] [JSStmt]
  | BinOp JSOp JSExp JSExp -- Unused; turn primitive ops into this
  | Neg JSExp -- Unused; turn calls to not into this
  | Var JSVar
  | Lit JSLit
  | Thunk [JSStmt] JSExp -- Statements + return expression = thunk
  | Eval JSExp
  | GetDataArg JSExp Int
    deriving Show

data JSLit
  = Num Double
  | Str String
  | Chr Char
    deriving Show

data JSOp
  = Add
  | Mul
  | Sub
  | Div
  | And
  | Or
  | Eq
  | Neq
  | LT
  | GT
  | LTE
  | GTE
    deriving Show
