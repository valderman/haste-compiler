module Data.JSTarget.Op where
import Prelude hiding (GT, LT)

data BinOp
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
    deriving (Eq)

instance Show BinOp where
  show Add    = "+"
  show Mul    = "*"
  show Sub    = "-"
  show Div    = "/"
  show Mod    = "%"
  show And    = "&&"
  show Or     = "||"
  show Eq     = "=="
  show Neq    = "!="
  show LT     = "<"
  show GT     = ">"
  show LTE    = "<="
  show GTE    = ">="
  show Shl    = "<<"
  show ShrL   = ">>>"
  show ShrA   = ">>"
  show BitAnd = "&"
  show BitOr  = "|"
  show BitXor = "^"

-- | Returns the precedence of the given operator as an int. Higher number
--   means higher priority.
opPrec :: BinOp -> Int
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
