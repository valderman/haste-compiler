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
  | StrictEq
  | Neq
  | StrictNeq
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
  show Add       = "+"
  show Mul       = "*"
  show Sub       = "-"
  show Div       = "/"
  show Mod       = "%"
  show And       = "&&"
  show Or        = "||"
  show Eq        = "=="
  show StrictEq  = "==="
  show Neq       = "!="
  show StrictNeq = "!=="
  show LT        = "<"
  show GT        = ">"
  show LTE       = "<="
  show GTE       = ">="
  show Shl       = "<<"
  show ShrL      = ">>>"
  show ShrA      = ">>"
  show BitAnd    = "&"
  show BitOr     = "|"
  show BitXor    = "^"

-- | Returns the precedence of the given operator as an int. Higher number
--   means higher priority.
opPrec :: BinOp -> Int
opPrec Mul       = 100
opPrec Div       = 100
opPrec Mod       = 100
opPrec Add       = 70
opPrec Sub       = 70
opPrec Shl       = 60
opPrec ShrA      = 60
opPrec ShrL      = 60
opPrec LT        = 50
opPrec GT        = 50
opPrec LTE       = 50
opPrec GTE       = 50
opPrec Eq        = 30
opPrec StrictEq  = 30
opPrec Neq       = 30
opPrec StrictNeq = 30
opPrec BitAnd    = 25
opPrec BitXor    = 24
opPrec BitOr     = 23
opPrec And       = 20
opPrec Or        = 10

-- | Is the given operator associative?
opIsAssoc :: BinOp -> Bool
opIsAssoc Mul    = True
opIsAssoc Add    = True
opIsAssoc BitAnd = True
opIsAssoc BitOr  = True
opIsAssoc BitXor = True
opIsAssoc And    = True
opIsAssoc Or     = True
opIsAssoc _      = False
