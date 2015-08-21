{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, FlexibleInstances, CPP,
             OverloadedStrings #-}
module Data.JSTarget.AST where
import qualified Data.Set as S
#if __GLASGOW_HASKELL__ >= 708
import qualified Data.Map.Strict as M
#else
import qualified Data.Map as M
#endif
import Data.JSTarget.Op
import qualified Data.ByteString as BS

type Arity = Int
type Comment = BS.ByteString
type Reorderable = Bool

-- | A Name consists of a variable name and optional (package, module)
--   information.
data Name = Name {
    nameIdent :: !BS.ByteString,
    nameQualifier :: !(Maybe (BS.ByteString, BS.ByteString))
  } deriving (Eq, Ord, Show)

class HasModule a where
  moduleOf :: a -> Maybe BS.ByteString
  pkgOf    :: a -> Maybe BS.ByteString

instance HasModule Name where
  moduleOf (Name _ mmod) = fmap snd mmod
  pkgOf (Name _ mmod)    = fmap fst mmod

instance HasModule Var where
  moduleOf (Foreign _)      = Nothing
  moduleOf (Internal n _ _) = moduleOf n
  pkgOf (Foreign _)         = Nothing
  pkgOf (Internal n _ _)    = pkgOf n

type KnownLoc = Bool

-- | Representation of variables.
data Var where
  Foreign  :: !BS.ByteString -> Var
  -- | Being a "known location" means that we can never substitute this
  --   variable for another one, as it is used to hold "return values" from
  --   case statements, tail loopification and similar.
  --   If a variable is *not* a known location, then we may always perform the
  --   substitution @a=b ; exp => exp [a/b]@.
  Internal :: !Name -> !Comment -> !KnownLoc -> Var
  deriving (Show)

isKnownLoc :: Var -> Bool
isKnownLoc (Internal _ _ knownloc) = knownloc
isKnownLoc _                       = False

instance Eq Var where
  {-# INLINE (==) #-}
  (Foreign f1)  == (Foreign f2)          = f1 == f2
  (Internal i1 _ _) == (Internal i2 _ _) = i1 == i2
  _ == _                                 = False

instance Ord Var where
  {-# INLINE compare #-}
  compare (Foreign f1) (Foreign f2)           = compare f1 f2
  compare (Internal i1 _ _) (Internal i2 _ _) = compare i1 i2
  compare (Foreign _) (Internal _ _ _)        = Prelude.LT
  compare (Internal _ _ _) (Foreign _)        = Prelude.GT

-- | Left hand side of an assignment. Normally we only assign internal vars,
--   but for some primops we need to assign array elements as well.
data LHS where
  -- | Introduce a new variable. May be reorderable.
  --   Invariant: a NewVar must be the first occurrence of a 'Var' in its
  --   scope.
  NewVar :: !Reorderable -> !Var -> LHS
  -- | Assign a value to an arbitrary LHS expression. May be reorderable.
  LhsExp :: !Reorderable -> !Exp -> LHS
  deriving (Eq, Show)

-- | Distinguish between normal, optimized and method calls.
--   Normal and optimized calls take a boolean indicating whether the called
--   function should trampoline or not. This defaults to True, and should
--   only be set to False when there is absolutely no possibility whatsoever
--   that the called function will tailcall.
data Call where
  Normal   :: !Bool          -> Call
  Fast     :: !Bool          -> Call
  Method   :: !BS.ByteString -> Call
  deriving (Eq, Show)

-- | Literals; nothing fancy to see here.
data Lit where
  LNum  :: !Double        -> Lit
  LStr  :: !BS.ByteString -> Lit
  LBool :: !Bool          -> Lit
  LInt  :: !Integer       -> Lit
  LNull :: Lit
  deriving (Eq, Show)

-- | Expressions. Completely predictable.
data Exp where
  Var       :: !Var -> Exp
  Lit       :: !Lit -> Exp
  -- | A literal JS snippet.
  --   Invariant: JSLits must not perform side effects or significant
  --   computation.
  JSLit     :: !BS.ByteString -> Exp
  Not       :: !Exp -> Exp
  BinOp     :: !BinOp -> Exp -> !Exp -> Exp
  Fun       :: ![Var] -> !Stm -> Exp
  Call      :: !Arity -> !Call -> !Exp -> ![Exp] -> Exp
  Index     :: !Exp -> !Exp -> Exp
  Arr       :: ![Exp] -> Exp
  AssignEx  :: !Exp -> !Exp -> Exp
  IfEx      :: !Exp -> !Exp -> !Exp -> Exp
  Eval      :: !Exp -> Exp
  Thunk     :: !Bool -> !Stm -> Exp -- Thunk may be updatable or not
  deriving (Eq, Show)

-- | Is the given expression guaranteed to not be a thunk?
--   @definitelyNotThunk e <=> safe to skip evaluation of e@
definitelyNotThunk :: Exp -> Bool
definitelyNotThunk (Lit {})   = True
definitelyNotThunk (JSLit {}) = True
definitelyNotThunk (Not {})   = True
definitelyNotThunk (BinOp {}) = True
definitelyNotThunk (Fun {})   = True
definitelyNotThunk (Arr {})   = True
definitelyNotThunk (Eval {})  = True
definitelyNotThunk _          = False

-- | Statements. The only mildly interesting thing here are the Case and Jump
--   constructors, which allow explicit sharing of continuations.
data Stm where
  Case     :: !Exp -> !Stm -> ![Alt] -> !Stm -> Stm
  Forever  :: !Stm -> Stm
  Assign   :: !LHS -> !Exp -> !Stm -> Stm
  Return   :: !Exp -> Stm
  Cont     :: Stm
  Stop     :: Stm -- Do nothing at all past this point
  Tailcall :: !Exp -> Stm
  ThunkRet :: !Exp -> Stm -- Return from a Thunk
  deriving (Eq, Show)

-- | Case alternatives - an expression to match and a branch.
type Alt = (Exp, Stm)

-- | Represents a module. A module has a name, an owning
--   package, a dependency map of all its definitions, and a bunch of
--   definitions.
data Module = Module {
    modPackageId   :: !BS.ByteString,
    modName        :: !BS.ByteString,
    modDeps        :: M.Map Name (S.Set Name),
    modDefs        :: M.Map Name Exp
  }

-- | Merge two modules. The module and package IDs of the second argument are
--   used, and the second argument will take precedence for symbols which exist
--   in both.
merge :: Module -> Module -> Module
merge m1 m2 = Module {
    modPackageId = modPackageId m2,
    modName = modName m2,
    modDeps = M.union (modDeps m1) (modDeps m2),
    modDefs = M.union (modDefs m1) (modDefs m2)
  }

-- | Imaginary module for foreign code that may need one.
foreignModule :: Module
foreignModule = Module {
    modPackageId   = "",
    modName        = "",
    modDeps        = M.empty,
    modDefs        = M.empty
  }

-- | An LHS that's guaranteed to not ever be read, enabling the pretty
--   printer to ignore assignments to it.
blackHole :: LHS
blackHole = LhsExp False $ Var blackHoleVar

-- | The variable of the blackHole LHS.
blackHoleVar :: Var
blackHoleVar = Internal (Name "" (Just ("$blackhole", "$blackhole"))) "" False

-- | Returns the precedence of the top level operator of the given expression.
--   Everything that's not an operator has equal precedence, higher than any
--   binary operator.
expPrec :: Exp -> Int
expPrec (BinOp Sub (Lit (LNum 0)) _) = 500 -- 0-n is always printed as -n
expPrec (BinOp op _ _)               = opPrec op
expPrec (AssignEx _ _)               = 0
expPrec (Not _)                      = 500
expPrec _                            = 1000
