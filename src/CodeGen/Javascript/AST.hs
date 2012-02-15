{-# LANGUAGE GeneralizedNewtypeDeriving, 
             TypeSynonymInstances,
             FlexibleInstances #-}
-- | Abstract syntax for the Javascript constructs needed to generate JS from
--   Core.
module CodeGen.Javascript.AST (
  JSVar (..), VarID, varZero, JSStmt (..), JSAlt (..), JSExp (..), JSLit (..),
  JSOp (..), opPrec, expPrec, lit) where
import Prelude hiding (LT, GT)

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
  | NativeCall String [JSExp]
  | NativeMethCall JSExp String [JSExp]
  | Fun [JSVar] [JSStmt]
  | BinOp JSOp JSExp JSExp
  | Neg JSExp
  | Not JSExp -- Bitwise negation; JS ~ operator
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

class Lit a where
  lit :: a -> JSExp

instance Lit Double where
  lit = Lit . Num

instance Lit String where
  lit = Lit . Str

instance Lit Char where
  lit = Lit . Chr

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
  | Shl
  | Shr
  | BitAnd
  | BitOr
  | BitXor
    deriving Show

-- | Returns the precedence of the given operator as an int. Higher number
--   means higher priority.
opPrec :: JSOp -> Int
opPrec Mul    = 100
opPrec Div    = 100
opPrec Add    = 70
opPrec Sub    = 70
opPrec Shl    = 60
opPrec Shr    = 60
opPrec LT     = 50
opPrec GT     = 50
opPrec LTE    = 50
opPrec GTE    = 50
opPrec Eq     = 30
opPrec Neq    = 30
opPrec BitAnd = 25
opPrec BitXor = 24
opPrec BitOr  = 23
opPrec And    = 20
opPrec Or     = 10

-- | Returns the precedence of the top level operator of the given expression.
--   Everything that's not an operator has equal precedence, higher than any
--   binary operator.
expPrec :: JSExp -> Int
expPrec (BinOp op _ _) = opPrec op
expPrec _              = 1000
