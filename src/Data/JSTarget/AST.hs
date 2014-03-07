{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Data.JSTarget.AST where
import qualified Data.Set as S
import qualified Data.Map as M
import System.IO.Unsafe
import System.Random (randomIO)
import Data.IORef
import Data.Word
import Control.Applicative
import Data.JSTarget.Op

type Arity = Int
type Comment = String
type Reorderable = Bool

-- | Shared statements.
newtype Shared a = Shared Lbl deriving (Eq, Show)

data Name = Name String (Maybe (String, String)) deriving (Eq, Ord, Show)

class HasModule a where
  moduleOf :: a -> Maybe String
  pkgOf    :: a -> Maybe String

instance HasModule Name where
  moduleOf (Name _ mmod) = fmap snd mmod
  pkgOf (Name _ mmod)    = fmap fst mmod

instance HasModule Var where
  moduleOf (Foreign _)    = Nothing
  moduleOf (Internal n _) = moduleOf n
  pkgOf (Foreign _)       = Nothing
  pkgOf (Internal n _)    = pkgOf n

-- | Representation of variables.
data Var where
  Foreign  :: String -> Var
  Internal :: Name -> Comment -> Var
  deriving (Show)

instance Eq Var where
  (Foreign f1)  == (Foreign f2)      = f1 == f2
  (Internal i1 _) == (Internal i2 _) = i1 == i2
  _ == _                             = False

instance Ord Var where
  compare (Foreign f1) (Foreign f2)       = compare f1 f2
  compare (Internal i1 _) (Internal i2 _) = compare i1 i2
  compare (Foreign _) (Internal _ _)      = Prelude.LT
  compare (Internal _ _) (Foreign _)      = Prelude.GT

-- | Left hand side of an assignment. Normally we only assign internal vars,
--   but for some primops we need to assign array elements as well.
--   LhsExp is never reorderable.
data LHS where
  NewVar :: Reorderable -> Var -> LHS
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
  LNull :: Lit
  deriving (Eq, Show)

-- | Expressions. Completely predictable.
data Exp where
  Var       :: Var -> Exp
  Lit       :: Lit -> Exp
  -- A verbatim JS expression. Must always be non-computing!
  Verbatim  :: String -> Exp
  Not       :: Exp -> Exp
  BinOp     :: BinOp -> Exp -> Exp -> Exp
  Fun       :: Maybe Name -> [Var] -> Stm -> Exp
  Call      :: Arity -> Call -> Exp -> [Exp] -> Exp
  Index     :: Exp -> Exp -> Exp
  Arr       :: [Exp] -> Exp
  AssignEx  :: Exp -> Exp -> Exp
  IfEx      :: Exp -> Exp -> Exp -> Exp
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
  NullRet :: Stm
  deriving (Eq, Show)

-- | Case alternatives - an expression to match and a branch.
type Alt = (Exp, Stm)

-- | Module fingerprint, containing a hash of compiler options and other things
--   that may be used to determine whether the module needs recompiling or not.
type Fingerprint = String

-- | Represents a module. A module has a name, a fingerprint, an owning
--   package, a dependency map of all its definitions, and a bunch of
--   definitions.
data Module = Module {
    modFingerprint :: !Fingerprint,
    modPackageId   :: !String,
    modName        :: !String,
    modDeps        :: !(M.Map Name (S.Set Name)),
    modDefs        :: !(M.Map Name (AST Exp))
  }

-- | Imaginary module for foreign code that may need one.
foreignModule :: Module
foreignModule = Module {
    modFingerprint = "",
    modPackageId   = "",
    modName        = "",
    modDeps        = M.empty,
    modDefs        = M.empty
  }

-- | An LHS that's guaranteed to not ever be read, enabling the pretty
--   printer to ignore assignments to it.
blackHole :: LHS
blackHole =
  LhsExp $ Var blackHoleVar

-- | The variable of the blackHole LHS.
blackHoleVar :: Var
blackHoleVar = Internal (Name "" (Just ("$blackhole", "$blackhole"))) ""

-- | An AST with local jumps.
data AST a = AST {
    astCode  :: a,
    astJumps :: JumpTable
  } deriving (Show, Eq)

instance Functor AST where
  fmap f (AST ast js) = AST (f ast) js

instance Applicative AST where
  pure = return
  (AST f js) <*> (AST x js') = AST (f x) (M.union js' js)

instance Monad AST where
  return x = AST x M.empty
  (AST ast js) >>= f =
    case f ast of
      AST ast' js' -> AST ast' (M.union js' js)

-- | Returns the precedence of the top level operator of the given expression.
--   Everything that's not an operator has equal precedence, higher than any
--   binary operator.
expPrec :: Exp -> Int
expPrec (BinOp Sub (Lit (LNum 0)) _) = 500 -- 0-n is always printed as -n
expPrec (BinOp op _ _)               = opPrec op
expPrec (Not _)                      = 500
expPrec _                            = 1000

type JumpTable = M.Map Lbl Stm

data Lbl = Lbl !Word64 !Word64 deriving (Eq, Ord, Show)

{-# NOINLINE nextLbl #-}
nextLbl :: IORef Word64
nextLbl = unsafePerformIO $ newIORef 0

{-# NOINLINE lblNamespace #-}
-- | Namespace for labels, to avoid collisions when combining modules.
--   We really ought to make this f(package, module) or something, but a random
--   64 bit unsigned int should suffice.
lblNamespace :: Word64
lblNamespace = unsafePerformIO $ randomIO

{-# NOINLINE lblFor #-}
-- | Produce a local reference to the given statement.
lblFor :: Stm -> AST Lbl
lblFor s = do
    (r, s') <- freshRef
    AST r (M.singleton r s')
  where
    freshRef = return $! unsafePerformIO $! do
      r <- atomicModifyIORef nextLbl $ \lbl ->
        lbl `seq` (lbl+1, Lbl lblNamespace lbl)
      -- We need to depend on s, or GHC will hoist us out of lblFor, possibly
      -- causing circular dependencies between expressions.
      return (r, s)
