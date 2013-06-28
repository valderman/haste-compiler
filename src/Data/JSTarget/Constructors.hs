{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
-- | User interface for the JSTarget AST.
module Data.JSTarget.Constructors where
import Data.JSTarget.AST
import Data.JSTarget.Op
import qualified Data.Map as M
import Control.Applicative

-- | Literal types.
class Literal a where
  lit :: a -> AST Exp

instance Literal Double where
  lit = pure . Lit . LNum

instance Literal Integer where
  lit = pure . Lit . LInt

instance Literal Bool where
  lit = pure . Lit . LBool

instance Literal [Char] where
  lit = pure . Lit . LStr

instance Literal a => Literal [a] where
  lit xs = Arr <$> mapM lit xs

instance Literal Exp where
  lit = pure

instance Literal Var where
  lit = pure . Var

-- | Shorthand for a continuation statement.
type StmCont = Stm -> Stm

-- | Create a foreign variable. Foreign vars will not be subject to any name
--   mangling.
foreignVar :: String -> Var
foreignVar = Foreign

-- | A regular, internal variable. Subject to name mangling.
internalVar :: Name -> String -> Var
internalVar = Internal

-- | A variable expression, for convenience.
var :: Name -> String -> AST Exp
var name comment = pure $ Var $ internalVar name comment

-- | Turn a Var into an expression.
varExp :: Var -> AST Exp
varExp = pure . Var

-- | Call to a native method on an object. Always saturated.
callMethod :: AST Exp -> String -> AST [Exp] -> AST Exp
callMethod obj meth args =
  Call 0 (Method meth) <$> obj <*> args

-- | Foreign function call. Always saturated.
callForeign :: String -> AST [Exp] -> AST Exp
callForeign f = fmap (Call 0 Fast (Var $ foreignVar f))

-- | A normal function call. May be unsaturated. A saturated call is always
--   turned into a fast call.
call :: Arity -> AST Exp -> AST [Exp] -> AST Exp
call arity f xs = do
  f' <- f
  xs' <- xs
  return $ foldApp $ Call (arity - length xs') Normal f' xs'

-- | "Fold" nested function applications into one, turning them into fast calls
--   if they turn out to be saturated.
foldApp :: Exp -> Exp
foldApp (Call arity Normal (Call _ Normal f args) args') =
  foldApp (Call arity Normal f (args ++ args'))
foldApp (Call arity Normal f args) | arity == 0 =
  Call arity Fast f args
foldApp ex =
  ex

-- | Create a thunk.
thunk :: AST Stm -> AST Exp
thunk = callForeign "T" . fmap ((:[]) . Fun [])

-- | Evaluate an expression that may or may not be a thunk.
eval :: AST Exp -> AST Exp
eval = callForeign "E" . fmap (:[])

-- | A binary operator.
binOp :: BinOp -> AST Exp -> AST Exp -> AST Exp
binOp op a b = BinOp op <$> a <*> b

-- | Negate an expression.
not_ :: AST Exp -> AST Exp
not_ = fmap Not

-- | Index into an array.
index :: AST Exp -> AST Exp -> AST Exp
index arr ix = Index <$> arr <*> ix

-- | Create a function.
fun :: [Var] -> AST Stm -> AST Exp
fun args = fmap (Fun args)

{-
-- | Case statement.
--   Takes a scrutinee expression, a default alternative, a list of more
--   specific alternatives, and a continuation statement. The continuation
--   will be explicitly shared among all the alternatives.
case_ :: AST Exp -> AST StmCont -> [(Lit, StmCont)] -> AST Stm -> AST Stm
case_ ex def alts cont =
    Case ex (def jump) [(x, s jump) | (x, s) <- alts] (Shared sharedRef)
  where
    sharedRef = lblFor cont
    jump      = Jump (Shared sharedRef)
-}

-- | Return from a function. Only statement that doesn't take a continuation.
ret :: AST Exp -> AST Stm
ret = fmap Return

-- | Create a new var with a new value.
newVar :: Var -> AST Exp -> AST StmCont
newVar lhs = liftA $ \rhs -> Assign (NewVar lhs) rhs

-- | Assignment without var.
assign :: AST Exp -> AST Exp -> AST StmCont
assign = liftA2 $ \lhs rhs -> Assign (LhsExp lhs) rhs
