{-# LANGUAGE GeneralizedNewtypeDeriving, 
             TypeSynonymInstances,
             FlexibleInstances, 
             DeriveGeneric #-}
-- | Abstract syntax for the Javascript constructs needed to generate JS from
--   Core.
module Haste.AST (
  JVar (..), JSVar (..), JName (..), JSName (..), JSStmt (..), JSAlt (..),
  JSExp (..), JSLit (..), JSOp (..), JSMod (..), JSLabel, opPrec, expPrec,
  lit, litN, defTag, defState, foreignModule, qualifiedName, bogusJSVar) where
import Prelude hiding (LT, GT)
import qualified Data.Map as M
import qualified Data.Set as S
import Module (ModuleName, mkModuleName, moduleNameString)
import GHC.Generics
import Data.Serialize
import Data.Serialize.Derive
import Control.Applicative

type JSLabel = String

data JSMod = JSMod {
    name :: !ModuleName,
    deps :: !(M.Map JSVar (S.Set JSVar)),
    code :: !(M.Map JSVar JSExp)
  }

-- | The imaginary module where all foreign functions live.
foreignModule :: JSMod
foreignModule = JSMod {
    name = mkModuleName "",
    deps = M.empty,
    code = M.empty
  }

instance Serialize JSMod where
  put (JSMod modName depends binds) = do
    put $ moduleNameString modName
    put depends
    put binds

  get =
    JSMod <$> (mkModuleName <$> get) <*> get <*> get

data JVar a = JSVar {
    jsmod  :: JSLabel,
    jsname :: JName a
  } deriving (Show, Ord, Eq, Generic)
type JSVar = JVar JSLabel

instance Functor JVar where
  fmap f (JSVar m n) = JSVar m (fmap f n)

bogusJSVar :: JSVar
bogusJSVar = JSVar "" (Internal "")

qualifiedName :: JSVar -> JSLabel
qualifiedName var =
  case jsmod var of 
    "" -> unique (jsname var)
    m  -> m ++ "." ++ unique (jsname var)

instance Serialize JSVar where
  get = deriveGet
  put = derivePut

data JName a
  = Foreign  {unique :: a}
  | External {unique :: a}
  | Internal {unique :: a}
    deriving (Show, Ord, Eq, Generic)
type JSName = JName JSLabel

instance Functor JName where
  fmap f (Foreign x)  = Foreign  $! f x
  fmap f (External x) = External $! f x
  fmap f (Internal x) = Internal $! f x

instance Serialize JSName where
  get = deriveGet
  put = derivePut

data JSStmt
  = Ret JSExp
  | While JSExp JSStmt -- Unused; use for CSE
  | Block [JSStmt]
  | Case JSExp [JSAlt]
  | If JSExp [JSStmt] [JSStmt]
  | NewVar JSExp JSExp
  | NamedFun String [JSVar] [JSStmt] -- Unused; turn top level defs into tihs
  | ExpStmt JSExp
  | LocalCopy [JSVar] [JSVar] [JSStmt] -- local copies, incoming vars, body
  | Continue
    deriving (Show, Eq, Generic)

instance Serialize JSStmt where
  get = deriveGet
  put = derivePut

data JSAlt
  = Cond JSExp [JSStmt]
  | Def        [JSStmt]
    deriving (Show, Eq, Generic)

instance Serialize JSAlt where
  get = deriveGet
  put = derivePut

data JSExp
  = Call JSExp [JSExp]
  | FastCall JSExp [JSExp]
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
  | Array [JSExp]
  | Assign JSExp JSExp
  | Index JSExp JSExp -- a[b] where a and b are the first and second JSExp
  | IfExp JSExp JSExp JSExp
  | DataCon JSExp [Bool]
  | Null
    deriving (Show, Eq, Generic)

instance Serialize JSExp where
  get = deriveGet
  put = derivePut

data JSLit
  = Num Double
  | Integer Integer
  | Str String
  | Chr Char
  | Boolean Bool
    deriving (Show, Eq, Generic)

instance Serialize JSLit where
  get = deriveGet
  put = derivePut

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

instance Lit Bool where
  lit = Lit . Boolean

instance Lit Double where
  lit = Lit . Num

instance Lit String where
  lit = Lit . Str

instance Lit Char where
  lit = Lit . Chr

instance Lit Integer where
  lit = Lit . Integer

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
    deriving (Show, Eq, Generic)

instance Serialize JSOp where
  get = deriveGet
  put = derivePut

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
