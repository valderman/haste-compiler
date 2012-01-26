-- | Module to generate Javascript from a ModGuts structure
module CodeGen.Javascript.Generate (generate) where
import GhcPlugins as P
import Name
import OccName
import TypeRep

import CodeGen.Javascript.Monad
import CodeGen.Javascript.AST as AST
import CodeGen.Javascript.Bag as Bag hiding (concat)

import Debug.Trace

generate :: ModGuts -> [JSStmt]
generate = genAST

genAST :: ModGuts -> [JSStmt]
genAST = concat . map (toList . snd . genJS . genBind) . mg_binds

genBind :: CoreBind -> JSGen ()
genBind (NonRec v ex) = do
  let (retEx, body) = genJS (genEx ex)
  v' <- genVar v
  emit $ Assign (AST.Var v') (Thunk (toList $ body) retEx)

genEx :: Expr Var -> JSGen JSExp
genEx (P.Var v) =
  genVar v >>= return . AST.Var
genEx (P.Lit lit) =
  genLit lit
genEx (App exp arg) = do
  -- TODO: the function's argument needs to be a thunk!
  exp' <- genEx exp
  arg' <- genEx arg
  return $ Call exp' [arg']
genEx (Lam v exp) =
  genFun [v] exp
genEx (Let bind exp) = do
  -- TODO: bindings can overwrite each other - disaster! Solve by actually
  --       generating names for vars rather than verbatim copy the Core names
  genBind bind
  genEx exp
genEx (P.Case exp v t alts) = do
  --   TODO: data constructor compares entire scrutinee to the constructor;
  --         figure out a good way to just look at the constructor while still
  --         using case construct.
  exp' <- genEx exp
  v' <- genVar v
  emit $ Assign (AST.Var v') (Eval exp')
  genAlts v' alts
genEx (Cast exp co) =
  genEx exp
genEx (Tick t ex) =
  genEx ex
genEx (Type t) =
  return $ AST.Var $ Named $ "TYPE: " ++ show t
genEx (Coercion co) =
  error "Don't know what to do with a coercion!"

genLit :: Literal -> JSGen JSExp
genLit lit = do
  return $ AST.Lit $ case lit of
    MachStr s      -> Str $ show s
    MachInt n      -> Num $ fromIntegral n
    MachFloat f    -> Num $ fromRational f
    MachDouble d   -> Num $ fromRational d
    MachChar c     -> Chr c
    LitInteger i _ -> Num $ fromIntegral i
    x              -> error $ "Literal: " ++ show x


-- | Generate code for a lambda and return it. Care is taken to ensure any and
--   all evaluation takes place within the function where it's actually
--   supposed to happen.
genFun :: [Var] -> Expr Var -> JSGen JSExp
genFun vs body = do
  vs' <- mapM genVar vs
  let (retExp, body') = genJS (genEx body)
  return $ Fun vs' (toList $ body' `snoc` Ret retExp)

-- | Generate all case alternatives for a given case expression. After 
--   evaluation of the expression, the return value of the case expression will
--   be bound to the variable the value of the scrutinee was bound to during
--   execution of the case expression, to enable implementing case expressions
--   as JS case statements while still being able to use them as expressions in
--   the JS AST.
genAlts :: JSVar -> [Alt P.Var] -> JSGen JSExp
genAlts v as = do
  as' <- mapM (genAlt v) (reverse as)
  emit $ AST.Case (AST.Var v) $ reverse as'
  return $ AST.Var v

-- | Generate a case alternative. Each alternative is responsible for binding
--   any constructor fields and generating any code needed to produce the alt's
--   result. That result is then bound to the AST.Var given.
--   Special care is taken to make sure any evaluation performed within the
--   alternative's expression is actually confined to the alternative's
--   generated code.
--
--   TODO: constructors are represented as strings. :(
genAlt :: JSVar -> Alt P.Var -> JSGen JSAlt
genAlt resultVar (con, binds, exp) = do
  con' <- case con of
    DEFAULT   -> return Def
    LitAlt l  -> genLit l >>= return . Cond
    DataAlt c -> return $ Cons (show c)
  let (retEx, body) = genJS (genBinds binds >> genEx exp)
  return . con' . toList $ body `snoc` Assign (AST.Var resultVar) retEx
  where
    -- Generate variables for all data constructor arguments, then bind the
    -- actual arguments to them. Only call wrapped in genJS, or these bindings
    -- will end up outside its respective case alternative, likely crashing the
    -- program.
    genBinds = sequence_ . zipWith genArgBind [1..]
    genArgBind num var = do
      var' <- genVar var
      emit $ (Assign (AST.Var var') (GetDataArg (AST.Var resultVar) num))

n_occ :: Name -> String
n_occ = occNameString . getOccName

instance Show Type where
  show (TyVarTy v) = show v
  show (AppTy a b) = show a ++ " (" ++ show b ++ ")"
  show (TyConApp t ts) = n_occ (tyConName t) ++ concat (map (\x -> ' ':show x) ts)
  show (FunTy a b) = "(" ++ show a ++ ") -> (" ++ show b ++ ")"
  show (ForAllTy v t) = "Forall " ++ show v ++ " " ++ show t

genVar :: Var -> JSGen JSVar
genVar = return . Named . show
