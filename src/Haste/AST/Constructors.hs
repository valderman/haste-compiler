{-# LANGUAGE FlexibleInstances, TupleSections, CPP, OverloadedStrings #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif
-- | Smart constructors for Haste's AST.
module Haste.AST.Constructors where
import Haste.AST.Syntax
import Haste.AST.Op
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

-- | Literal types.
class Literal a where
  lit :: a -> Exp

instance Literal Lit where
  lit = Lit

instance Literal Double where
  lit = lit . LNum

instance Literal Integer where
  lit = lit . LInt

instance Literal Bool where
  lit = lit . LBool

instance Literal BS.ByteString where
  lit = lit . LStr

instance Literal [Char] where
  lit = lit . BS.fromString

#if __GLASGOW_HASKELL__ < 710
instance Literal a => Literal [a] where
#else
instance {-# OVERLAPPABLE #-} Literal a => Literal [a] where
#endif
  lit = Arr . map lit

instance Literal Exp where
  lit = id

instance Literal Var where
  lit = Var

litN :: Double -> Exp
litN = lit

litS :: BS.ByteString -> Exp
litS = lit

-- | Create a foreign variable. Foreign vars will not be subject to any name
--   mangling.
foreignVar :: BS.ByteString -> Var
foreignVar = Foreign

-- | A regular, internal variable. Subject to name mangling.
internalVar :: Name -> BS.ByteString -> Var
internalVar n c = Internal n c False

-- | A variable serving as a known location, to store return values from
--   expressions that get compiled into statements.
knownLocation :: Name -> BS.ByteString -> Var
knownLocation n c = Internal n c True

-- | Create a name, qualified or not.
name :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString) -> Name
name = Name

-- | A variable expression, for convenience.
var :: Name -> BS.ByteString -> Exp
var n comment = Var $ internalVar n comment

-- | Turn a Var into an expression.
varExp :: Var -> Exp
varExp = Var

-- | Call to a native method on an object. Always saturated.
callMethod :: Exp -> BS.ByteString -> [Exp] -> Exp
callMethod obj meth args = Call 0 (Method meth) obj args

-- | Foreign function call. Always saturated, never trampolines.
callForeign :: BS.ByteString -> [Exp] -> Exp
callForeign f = Call 0 (Fast False) (Var $ foreignVar f)

-- | A normal function call. May be unsaturated. A saturated call is always
--   turned into a fast call.
call :: Arity -> Exp -> [Exp] -> Exp
call arity f xs = foldApp $ Call (arity - length xs) (Normal True) f xs

callSaturated :: Exp -> [Exp] -> Exp
callSaturated f xs = Call 0 (Fast True) f xs

-- | "Fold" nested function applications into one, turning them into fast calls
--   if they turn out to be saturated.
foldApp :: Exp -> Exp
foldApp (Call arity (Normal tramp) (Call _ (Normal _) f args) args') =
  Call arity (Normal tramp) (foldApp f) (args ++ args')
foldApp (Call 0 (Normal tramp) f args) =
  Call 0 (Fast tramp) f args
foldApp (Call arity (Normal tramp) f args) | arity > 0 =
    Fun  newargs $ Return
                 $ Call arity (Fast tramp) f (args ++ map Var newargs)
  where
    newargs = newVars "_fa_" arity
foldApp ex =
  ex

-- | Introduce n new vars.
newVars :: String -> Int -> [Var]
newVars prefix n =
    map nv [1..n]
  where
    nv i = Internal (Name (BS.fromString $ prefix++show i) Nothing) "" False

-- | Create a thunk.
thunk :: Bool -> Stm -> Exp
thunk = Thunk

-- | Evaluate an expression that may or may not be a thunk.
eval :: Exp -> Exp
eval ex
  | definitelyNotThunk ex = ex
  | otherwise             = Eval ex

-- | Create a tail call.
tailcall :: Exp -> Stm
tailcall = Tailcall

-- | A binary operator.
binOp :: BinOp -> Exp -> Exp -> Exp
binOp = BinOp

-- | Negate an expression.
not_ :: Exp -> Exp
not_ = Not

-- | Index into an array.
index :: Exp -> Exp -> Exp
index = Index

-- | Create a function.
fun :: [Var] -> Stm -> Exp
fun = Fun

-- | Create an array of expressions.
array :: [Exp] -> Exp
array = Arr

-- | Case statement.
--   Takes a scrutinee expression, a default alternative, a list of more
--   specific alternatives, and a continuation statement. The continuation
--   will be explicitly shared among all the alternatives.
case_ :: Exp -> (Stm -> Stm) -> [(Exp, Stm -> Stm)] -> Stm -> Stm
case_ ex def alts = Case ex (def stop) (map (\(e, s) -> (e, s stop)) alts)

-- | Return from a function.
ret :: Exp -> Stm
ret = Return

-- | Return from a thunk.
thunkRet :: Exp -> Stm
thunkRet = ThunkRet

-- | Create a new var with a new value.
newVar :: Reorderable -> Var -> Exp -> Stm -> Stm
newVar r lhs = Assign (NewVar r lhs)

-- | Reuse an old variable.
assignVar :: Reorderable -> Var -> Exp -> Stm -> Stm
assignVar r lhs = Assign (LhsExp r (Var lhs))

-- | Assignment without var. Performed for the side effect, so never
--   reorderable.
sideEffectingAssign :: Exp -> Exp -> Stm -> Stm
sideEffectingAssign lhs = Assign (LhsExp False lhs)

-- | Assignment expression.
assignEx :: Exp -> Exp -> Exp
assignEx = AssignEx

-- | Terminate a statement without doing anything at all.
stop :: Stm
stop = Stop
