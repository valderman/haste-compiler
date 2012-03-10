{-# LANGUAGE GeneralizedNewtypeDeriving, 
             TypeSynonymInstances,
             FlexibleInstances #-}
-- | Abstract syntax for the Javascript constructs needed to generate JS from
--   Core.
module CodeGen.Javascript.AST (
  JSVar (..), JSStmt (..), JSAlt (..), JSExp (..), JSLit (..),
  JSOp (..), JSMod (..), opPrec, expPrec, lit, litN, defTag, defState) where
import Prelude hiding (LT, GT)
import qualified Data.Map as M
import Module (ModuleName)

type JSLabel = String

data JSMod = JSMod {
    name :: ModuleName,
    deps :: M.Map JSVar JSVar,
    code :: M.Map JSVar JSExp
  }

data JSVar
  = Foreign JSLabel
  | External {unique :: JSLabel, external :: JSLabel}
  | Internal JSLabel
    deriving (Show, Ord, Eq)

data JSStmt
  = Ret JSExp
  | CallRet JSExp [JSExp] -- Unused; use for CSE
  | While JSExp JSStmt -- Unused; use for CSE
  | Block [JSStmt]
  | Case JSExp [JSAlt]
  | NewVar JSExp JSExp
  | NamedFun String [JSVar] [JSStmt] -- Unused; turn top level defs into tihs
    deriving Show

data JSAlt
  = Cond JSExp [JSStmt]
  | Def        [JSStmt]
  | Cons Int   [JSStmt]
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
  | Array [JSExp]
  | Assign JSExp JSExp
  | Index JSExp JSExp -- a[b] where a and b are the first and second JSExp
    deriving Show

data JSLit
  = Num Double
  | Str String
  | Chr Char
    deriving Show

-- | First tag for data constructors
defTag :: JSExp
defTag = litN 0

-- | Represents the state argument passed around by state monads like IO and
--   ST. However, since this argument is never examined, anything can be used
--   for it; defState is just a convenience value, when you have to explicitly
--   put some value for the state.
defState :: JSExp
defState = litN 0

class Lit a where
  lit :: a -> JSExp

instance Lit Double where
  lit = Lit . Num

instance Lit String where
  lit = Lit . Str

instance Lit Char where
  lit = Lit . Chr

litN :: Double -> JSExp
litN = lit

data JSOp
  = Add
  | Mul
  | Sub
  | Div
  | Mod
  | And
  | Or
  | Eq
  | Neq
  | LT
  | GT
  | LTE
  | GTE
  | Shl
  | ShrL
  | ShrA
  | BitAnd
  | BitOr
  | BitXor
    deriving Show

-- | Returns the precedence of the given operator as an int. Higher number
--   means higher priority.
opPrec :: JSOp -> Int
opPrec Mul    = 100
opPrec Div    = 100
opPrec Mod    = 100
opPrec Add    = 70
opPrec Sub    = 70
opPrec Shl    = 60
opPrec ShrA   = 60
opPrec ShrL   = 60
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
