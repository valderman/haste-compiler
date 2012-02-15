-- | Module to generate Javascript from a ModGuts structure
module CodeGen.Javascript.Generate (generate) where
import GhcPlugins as P
import PrimOp
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
  v' <- genVar v
  ex' <- genThunk ex
  emit $ Assign (AST.Var v') ex'
genBind (Rec bs) =
  mapM_ (\(v, ex) -> genBind (NonRec v ex)) bs

isUnliftedExp :: Expr Var -> Bool
isUnliftedExp = isStrictType . exprType

isVar :: Expr Var -> Bool
isVar (P.Var _) = True
isVar _         = False

-- | Generate a thunk, but only if appropriate. Generating a thunk is
--   appropriate unless the expression:
--   * has an unlifted type;
--   * has a lifted type but is a variable, which is then obviously a thunk;
--   * is a function;
--   * is a type class dictionary;
--   * is already in HNF.
--   Not generating a thunk for expressions already in HNF but otherwise
--   subject to thunking is only OK as long as the eval operation is able to
--   cope with getting passed non-thunks; if this were to change, we must
--   generate thunks even for HNF expressions!
genThunk :: Expr Var -> JSGen JSExp
genThunk exp
  | isUnliftedExp exp = do
    genEx exp
  | isVar exp = do
    genEx exp
  | isFunTy . dropForAlls . exprType $ exp = do
    genEx exp
  | isTyVarTy $ exprType exp = do
    genEx exp
  | exprIsHNF exp = do
    genEx exp
  | otherwise = do
    let (exp', supportStmts) = genJS $ genEx exp
    return $ Thunk (toList supportStmts) exp'

-- | Generate an eval expression, where needed. Unlifted types don't need it.
genEval :: Expr Var -> JSGen JSExp
genEval exp
  | isUnliftedExp exp = do
    genEx exp
  | otherwise = do
    genEx exp >>= return . Eval 

genEx :: Expr Var -> JSGen JSExp
genEx (P.Var v) = do
  case idDetails v of
    DataConWorkId dc -> genDataCon dc
    DataConWrapId dc -> genDataCon dc
    _                -> genVar v >>= return . AST.Var
genEx (P.Lit lit) =
  genLit lit
genEx (App exp arg) = do
  arg' <- genThunk arg
  poExp <- genPrimOp exp arg'
  case poExp of
    Just primop -> do
      return primop
    _ -> do
      exp' <- genEx exp
      return $ Call exp' [arg']
genEx (Lam v exp) =
  genFun [v] exp
genEx (Let bind exp) = do
  -- TODO: bindings can overwrite each other - disaster! Solve by actually
  --       generating names for vars rather than verbatim copy the Core names
  genBind bind
  genEx exp
genEx (P.Case exp v t alts) =
  genCase exp v t alts
genEx (Cast exp co) =
  genEx exp
genEx (Tick t ex) =
  genEx ex
genEx (Type t) =
  return $ AST.Var $ NamedStrict $ show t
genEx (Coercion co) =
  error "Don't know what to do with a coercion!"

-- | Generate code for the given data constructor
genDataCon :: DataCon -> JSGen JSExp
genDataCon dc = do
  return $ NativeCall "D" [
    lit $ (fromIntegral $ dataConTag dc :: Double),
    lit $ (fromIntegral $ dataConSourceArity dc :: Double)]

-- | Generate an expression for the given primitive operation. If the given
--   expression isn't a primitive operation, return Nothing.
genPrimOp :: Expr Var -> JSExp -> JSGen (Maybe JSExp)
genPrimOp (P.Var id) arg
  | PrimOpId op <- idDetails id = do
    return . Just $ unOp op arg
genPrimOp (App (P.Var id) arg2) arg1
  | PrimOpId op <- idDetails id = do
    arg2' <- genThunk arg2
    return . Just $ binOp op arg1 arg2'
genPrimOp _ _ =
  return Nothing

unOp :: PrimOp -> JSExp -> JSExp
unOp op x =
  case op of
    IntNegOp       -> Neg x
    ChrOp          -> NativeCall "String.fromCharCode" [x]
    OrdOp          -> NativeMethCall x "charCodeAt" [lit (0::Double)]
    Word2IntOp     -> x
    Int2WordOp     -> x
    Int2FloatOp    -> x
    Int2DoubleOp   -> x
    Double2IntOp   -> x
    Double2FloatOp -> x
    Float2IntOp    -> x
    Float2DoubleOp -> x
    NotOp          -> Not x
    x              -> error $ "Unsupported operation: " ++ show x

binOp :: PrimOp -> JSExp -> JSExp -> JSExp
binOp op a b =
  op' a b
  where
    call f a b = NativeCall f [a, b]
    op' = case op of
      IntAddOp -> BinOp Add
      IntSubOp -> BinOp Sub
      IntMulOp -> BinOp Mul
      IntMulMayOfloOp -> BinOp Mul -- This is correct, but slow!
      IntQuotOp -> call "quot"
      IntRemOp -> BinOp Mod -- Javascript % operator is actually rem, not mod!
      IntAddCOp -> call "addC"
      IntSubCOp -> call "subC"
      IntGtOp -> BinOp AST.GT
      IntGeOp -> BinOp GTE
      IntLtOp -> BinOp AST.LT
      IntLeOp -> BinOp LTE
      IntEqOp -> BinOp Eq
      IntNeOp -> BinOp Neq
      x       -> error $ "Unsupported operation: " ++ show x

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

genCase :: Expr Var -> Var -> Type -> [Alt P.Var] -> JSGen JSExp
genCase exp v t alts = do
  v' <- genVar v
  exp' <- genEval exp
  emit $ Assign (AST.Var v') exp'
  genAlts v' alts


-- | Generate all case alternatives for a given case expression. After 
--   evaluation of the expression, the return value of the case expression will
--   be bound to the variable the value of the scrutinee was bound to during
--   execution of the case expression, to enable implementing case expressions
--   as JS case statements while still being able to use them as expressions in
--   the JS AST.
--   If the case expression only has one alternative, omit the case part
--   entirely, as we're obviously just picking data apart.
genAlts :: JSVar -> [Alt P.Var] -> JSGen JSExp
genAlts v [a] = do
  genAlt v a >>= mapM_ emit . getStmts
  return $ AST.Var v
  where
    getStmts (Cond _ ss) = ss
    getStmts (Def ss)    = ss
    getStmts (Cons _ ss) = ss
genAlts v as = do
  -- Core requires DEFAULT alts to come first, but we want them last in our JS.
  (a':as') <- mapM (genAlt v) as
  emit $ AST.Case (AST.Var v) $ as' ++ [a']
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
    DataAlt c -> return . Cons $ dataConTag c
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
  show (TyVarTy v) = if isExternalName (P.varName v)
                       then showPpr $ P.varName v
                       else showPpr . nameUnique $ P.varName v
  show (AppTy a b) = show a ++ " (" ++ show b ++ ")"
  show (TyConApp t ts) = n_occ (tyConName t) ++ concat (map (\x -> ' ':show x) ts)
  show (FunTy a b) = "(" ++ show a ++ ") -> (" ++ show b ++ ")"
  show (ForAllTy v t) = "Forall " ++ show v ++ " " ++ show t

genVar :: Var -> JSGen JSVar
genVar v = do
  return . named . nameType $ P.varName v
  where
    named = if isUnliftedExp (P.Var v) then NamedStrict else NamedLazy
    nameType = if isExternalName (P.varName v)
                 then showPpr
                 else showPpr . nameUnique
