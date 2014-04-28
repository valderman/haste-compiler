{-# LANGUAGE FlexibleInstances, OverlappingInstances, TupleSections #-}
-- | User interface for the JSTarget AST.
module Data.JSTarget.Constructors where
import Data.JSTarget.AST
import Data.JSTarget.Op
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

litN :: Double -> AST Exp
litN = lit

-- | Create a foreign variable. Foreign vars will not be subject to any name
--   mangling.
foreignVar :: String -> Var
foreignVar = Foreign

-- | A regular, internal variable. Subject to name mangling.
internalVar :: Name -> String -> Var
internalVar = Internal

-- | Create a name, qualified or not.
name :: String -> Maybe (String, String) -> Name
name = Name

-- | A variable expression, for convenience.
var :: Name -> String -> AST Exp
var n comment = pure $ Var $ internalVar n comment

-- | Turn a Var into an expression.
varExp :: Var -> AST Exp
varExp = pure . Var

-- | Call to a native method on an object. Always saturated.
callMethod :: AST Exp -> String -> [AST Exp] -> AST Exp
callMethod obj meth args =
  Call 0 (Method meth) <$> obj <*> sequence args

-- | Foreign function call. Always saturated.
callForeign :: String -> [AST Exp] -> AST Exp
callForeign f = fmap (Call 0 Fast (Var $ foreignVar f)) . sequence

-- | A normal function call. May be unsaturated. A saturated call is always
--   turned into a fast call.
call :: Arity -> AST Exp -> [AST Exp] -> AST Exp
call arity f xs = do
  foldApp <$> (Call (arity - length xs) Normal <$> f <*> sequence xs)

callSaturated :: AST Exp -> [AST Exp] -> AST Exp
callSaturated f xs = Call 0 Fast <$> f <*> sequence xs

-- | "Fold" nested function applications into one, turning them into fast calls
--   if they turn out to be saturated.
foldApp :: Exp -> Exp
foldApp (Call arity Normal (Call _ Normal f args) args') =
  Call arity Normal (foldApp f) (args ++ args')
foldApp (Call 0 Normal f args) =
  Call 0 Fast f args
foldApp (Call arity Normal f args) | arity > 0 =
    Fun Nothing newargs $ Return $ Call arity Fast f (args ++ map Var newargs)
  where
    newargs = newVars "_fa_" arity
foldApp ex =
  ex

-- | Introduce n new vars.
newVars :: String -> Int -> [Var]
newVars prefix n =
    map newVar [1..n]
  where
    newVar i = Internal (Name (prefix ++ show i) Nothing) ""

-- | Create a thunk.
thunk :: AST Stm -> AST Exp
thunk stm = callForeign "new T" [Fun Nothing [] <$> stm]

-- | Unpack the given expression if it's a thunk without internal bindings.
fromThunk :: AST Exp -> Maybe (AST Exp)
fromThunk (AST (Call 0 Fast (Var (Foreign "new T")) [body]) js) =
  case body of
    Fun Nothing [] (Return ex) -> Just (AST ex js)
    _                          -> Nothing
fromThunk _ =
  Nothing

-- | Returns True if the given expression causes evaluation by appearing
--   outside a closure, otherwise False.
evaluates :: Exp -> Bool
evaluates (Var _)        = False
evaluates (Lit _)        = False
evaluates (Not ex)       = evaluates ex
evaluates (BinOp _ a b)  = evaluates a || evaluates b
evaluates (Fun _ _ _)    = False
evaluates (Call _ _ _ _) = True
evaluates (Index a b)    = evaluates a || evaluates b
evaluates (Arr xs)       = any evaluates xs
evaluates (AssignEx a b) = evaluates a || evaluates b
evaluates (IfEx c a b)   = evaluates c || evaluates a || evaluates b

-- | Evaluate an expression that may or may not be a thunk.
eval :: AST Exp -> AST Exp
eval = callForeign "E" . (:[])

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
fun args = fmap (Fun Nothing args)

-- | Create an array of expressions.
array :: [AST Exp] -> AST Exp
array = fmap Arr . sequence

-- | Case statement.
--   Takes a scrutinee expression, a default alternative, a list of more
--   specific alternatives, and a continuation statement. The continuation
--   will be explicitly shared among all the alternatives.
case_ :: AST Exp
      -> (AST Stm -> AST Stm)
      -> [(AST Exp, AST Stm -> AST Stm)]
      -> AST Stm
      -> AST Stm
case_ ex def alts cont = do
  ex' <- ex
  shared <- cont >>= lblFor
  let jmp = pure $ Jump (Shared shared)
  def' <- def jmp
  alts' <- sequence [(,) <$> x <*> s jmp | (x, s) <- alts]
  pure $ Case ex' def' alts' (Shared shared)

-- | Return from a function. Only statement that doesn't take a continuation.
ret :: AST Exp -> AST Stm
ret = fmap Return

-- | Create a new var with a new value.
newVar :: Reorderable -> Var -> AST Exp -> AST Stm -> AST Stm
newVar r lhs = liftA2 $ \rhs -> Assign (NewVar r lhs) rhs

-- | Assignment without var.
assign :: AST Exp -> AST Exp -> AST Stm -> AST Stm
assign = liftA3 $ \lhs rhs -> Assign (LhsExp lhs) rhs

-- | Assignment expression.
assignEx :: AST Exp -> AST Exp -> AST Exp
assignEx = liftA2 AssignEx

-- | Terminate a statement without doing anything at all.
nullRet :: AST Stm
nullRet = pure NullRet
