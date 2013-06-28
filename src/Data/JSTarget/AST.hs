{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Data.JSTarget.AST where
import qualified Data.Set as S
import qualified Data.Map as M
import System.IO.Unsafe
import Data.IORef
import Control.Applicative
import Control.Monad (ap)
import Data.JSTarget.Op

type Arity = Int
type Comment = String

newtype Shared a = Shared Lbl deriving (Eq, Show)

newtype Name = Name Int deriving (Enum, Eq, Ord, Show)

zeroName :: Name
zeroName = Name 0

-- | Representation of variables. Parametrized because we sometimes want to
--   disallow foreign vars.
data Var where
  Foreign  :: String -> Var
  Internal :: Name -> Comment -> Var
  deriving (Eq, Show)

-- | Left hand side of an assignment. Normally we only assign internal vars,
--   but for some primops we need to assign array elements as well.
data LHS where
  NewVar :: Var -> LHS
  LhsExp :: Exp -> LHS
  deriving (Eq, Show)

-- | Distinguish between normal, optimized and method calls.
data Call where
  Normal   :: Call
  Fast     :: Call
  Method   :: String -> Call
  deriving (Eq, Show)

-- | Literals; nothing fancy to see here.
data Lit where
  LNum  :: Double  -> Lit
  LStr  :: String  -> Lit
  LBool :: Bool    -> Lit
  LInt  :: Integer -> Lit
  deriving (Eq, Show)

-- | Expressions. Completely predictable.
data Exp where
  Var   :: Var -> Exp
  Lit   :: Lit -> Exp
  Not   :: Exp -> Exp
  BinOp :: BinOp -> Exp -> Exp -> Exp
  Fun   :: [Var] -> Stm -> Exp
  Call  :: Arity -> Call -> Exp -> [Exp] -> Exp
  Index :: Exp -> Exp -> Exp
  Arr   :: [Exp] -> Exp
  deriving (Eq, Show)

-- | Statements. The only mildly interesting thing here are the Case and Jump
--   constructors, which allow explicit sharing of continuations.
data Stm where
  Case    :: Exp -> Stm -> [Alt] -> Shared Stm -> Stm
  Forever :: Stm -> Stm
  Assign  :: LHS -> Exp -> Stm -> Stm
  Return  :: Exp -> Stm
  Cont    :: Stm
  Jump    :: Shared Stm -> Stm
  deriving (Eq, Show)

-- | Case alternatives - a literal and a branch.
type Alt = (Lit, Stm)

-- | Represents a module. A module has a name, a dependency map of all its
--   definitions, and a bunch of definitions.
data Module = Module {
    name :: String,
    deps :: M.Map Name (S.Set Name),
    defs :: M.Map Name Exp
  }

-- | An AST with local jumps.
newtype AST a = AST {unAST :: JumpTable -> (a, JumpTable)}

instance Functor AST where
  fmap f (AST ast) = AST $ \m ->
    case ast m of (x, m') -> (f x, m')

instance Applicative AST where
  pure  = return
  (<*>) = ap

instance Monad AST where
  return x        = AST $ \m -> (x, m)
  (AST ast) >>= f = AST $ \m ->
    case ast m of
      (x, m') -> unAST (f x) m'

-- | Returns the precedence of the top level operator of the given expression.
--   Everything that's not an operator has equal precedence, higher than any
--   binary operator.
expPrec :: Exp -> Int
expPrec (BinOp op _ _) = opPrec op
expPrec (Not _)        = 500
expPrec _              = 1000

type JumpTable = M.Map Lbl Stm

getRef :: Lbl -> AST Stm
getRef lbl = AST $ \m -> (m M.! lbl, m)

putRef :: Lbl -> Stm -> AST ()
putRef lbl stm = AST $ \m -> ((), M.insert lbl stm m)

-- | Extract all shared code paths from an AST.
jumps :: AST ast -> JumpTable
jumps = snd . freeze

-- | Extract the actual AST, sans jumps.
code :: AST ast -> ast
code = fst . freeze

-- | Freeze the AST, producing data that's suitable for reading but a pain to
--   modify.
freeze :: AST ast -> (ast, JumpTable)
freeze (AST ast) = ast M.empty

-- | Turn a frozen AST back into one that's convenient to modify but slightly
--   less convenient to read.
unfreeze :: (ast, JumpTable) -> AST ast
unfreeze = AST . const

newtype Lbl = Lbl Int deriving (Eq, Ord, Show)

{-# NOINLINE nextLbl #-}
nextLbl :: IORef Int
nextLbl = unsafePerformIO $ newIORef 0

{-# NOINLINE lblFor #-}
lblFor :: AST Stm -> Lbl
lblFor s = unsafePerformIO $ do
  atomicModifyIORef' nextLbl (\lbl -> (lbl+1, Lbl lbl))
