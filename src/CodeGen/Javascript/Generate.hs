-- | Module to generate Javascript from a ModGuts structure
module CodeGen.Javascript.Generate (generate) where
import GhcPlugins as P
import Name
import OccName
import TypeRep
import PrimOp (PrimOp)
import ForeignCall
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (foldl')

import CodeGen.Javascript.Monad
import CodeGen.Javascript.AST as AST
import Bag
import CodeGen.Javascript.PrimOps
import CodeGen.Javascript.Module
import CodeGen.Javascript.PrintJS (prettyJS)

-- | Turn a pile of Core into our intermediate JS AST.
generate :: ModGuts -> JSMod
generate mg =
  JSMod {
      name = moduleName $ mg_module mg,
      deps = foldl' insDep M.empty theMod,
      code = foldl' insFun M.empty theMod
    }
  where
    theMod = genAST mg
    insFun m (_, NewVar (AST.Var name) fun) =
      M.insert name fun m
    
    insDep m (deps, NewVar (AST.Var name) _) =
      M.insert name deps m

genAST :: ModGuts -> [(S.Set JSVar, JSStmt)]
genAST =
  map (depsAndCode . genJS . genBind) . concatMap unRec . mg_binds
  where
    depsAndCode (_, deps, code) =
      case bagToList code of
        [code'] ->
          (deps, code')
        lotsOfCode ->
          error $  "Single top level exp generated several assignments!\n"
                ++ prettyJS lotsOfCode

-- | Turn a recursive binding into a list of non-recursive ones.
unRec :: CoreBind -> [CoreBind]
unRec (Rec bs) = unzipWith NonRec bs
unRec b        = [b]

-- | Generate code for all bindings. genBind spits out an error if it receives
--   a recursive binding; this is because it's quite a lot easier to keep track
--   of which functions depend on each other if every genBind call results in a
--   single function being generated.
--   Use `genBindRec` to generate code for local potentially recursive bindings 
--   as their dependencies get merged into their parent's anyway.
genBind :: CoreBind -> JSGen ()
genBind (NonRec v ex) = do
  v' <- genVar v
  ex' <- genThunk ex
  emit $ NewVar (AST.Var v') ex'
genBind (Rec bs) =
  error $  "genBind got recursive bindings: "
        ++ showPpr bs

-- | Generate code for a potentially recursive binding. Only use this for local
--   functions; use `genBind` for top level bindings instead.
genBindRec :: CoreBind -> JSGen ()
genBindRec bs@(Rec _) = mapM_ genBind (unRec bs)
genBindRec b          = genBind b

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
    let (exp', deps, supportStmts) = genJS $ genEx exp
    dependOn deps
    return $ Thunk (bagToList supportStmts) exp'

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
  genApp exp arg >>= return . foldUpApp
genEx (Lam v exp) =
  genFun [v] exp >>= return . foldUpFun
genEx (Let bind exp) = do
  genBindRec bind
  genEx exp
genEx (P.Case exp v t alts) =
  genCase exp v t alts
genEx (Cast exp co) =
  genEx exp
genEx (Tick t ex) =
  genEx ex
genEx (Type t) =
  error "Type annotation encountered where it shouldn't be!"
genEx (Coercion co) =
  error "Don't know what to do with a coercion!"

-- | Generate code for funcion application
genApp :: Expr Var -> Arg Var -> JSGen JSExp
genApp exp (Type _) = do
  -- Discard type annotations
  genEx exp
genApp exp arg = do 
  arg' <- genThunk arg
  poExp <- genPrimOp exp [arg']
  case poExp of
    Just primop -> do
      return primop
    _ -> do
      exp' <- genEx exp
      return $ Call exp' [arg']

-- | Fold up nested function applications into a single call as far as
--   possible.
foldUpApp :: JSExp -> JSExp
foldUpApp (Call f as) =
  case foldUpApp f of
    Call f' as' -> Call f' (as' ++ as)
    f'          -> Call f' as
foldUpApp exp =
  exp

-- | Generate code for the given data constructor
genDataCon :: DataCon -> JSGen JSExp
genDataCon dc = do
  return $ NativeCall "D" [
    lit $ (fromIntegral $ dataConTag dc :: Double),
    Array $ map strict (dataConRepStrictness dc)]
  where
    strict MarkedStrict = lit (1 :: Double)
    strict _            = lit (0 :: Double)

-- | Generate an expression for the given primitive operation. If the given
--   expression isn't a primitive operation, return Nothing.
genPrimOp :: Expr Var -> [JSExp] -> JSGen (Maybe JSExp)
genPrimOp (P.Var id) xs | PrimOpId op <- idDetails id =
  return $ Just $ genOp op xs
genPrimOp (App f x) xs
  | Type _ <- x =
    genPrimOp f xs
  | otherwise = do
    x' <- genThunk x
    genPrimOp f (x':xs)
genPrimOp _ _ =
  return Nothing

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
genFun [v] body | isTyVar v =
  genEx body
genFun vs body = do
  vs' <- mapM genVar vs
  let (retExp, deps, body') = genJS (genEx body)
  dependOn deps
  return $ Fun vs' (bagToList $ body' `snocBag` Ret retExp)

-- | Fold up nested lambdas into a single function as far as possible.
foldUpFun :: JSExp -> JSExp
foldUpFun (Fun vs [Ret (Fun vs' b)]) =
  foldUpFun $ Fun (vs ++ vs') b
foldUpFun exp =
  exp

genCase :: Expr Var -> Var -> Type -> [Alt P.Var] -> JSGen JSExp
genCase exp v t alts = do
  v' <- genVar v
  exp' <- genEval exp
  emit $ NewVar (AST.Var v') exp'
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
  let (retEx, deps, body) = genJS (genBinds binds >> genEx exp)
  dependOn deps
  return . con' . bagToList $ body `snocBag` NewVar (AST.Var resultVar) retEx
  where
    -- Generate variables for all data constructor arguments, then bind the
    -- actual arguments to them. Only call wrapped in genJS, or these bindings
    -- will end up outside its respective case alternative, likely crashing the
    -- program.
    genBinds = sequence_ . zipWith genArgBind [1..] . filter (not . isTyVar)
    genArgBind num var = do
      var' <- genVar var
      emit $ (NewVar (AST.Var var') (GetDataArg (AST.Var resultVar) num))

-- | Extracts the name of a foreign var.
foreignName :: ForeignCall -> String
foreignName (CCall (CCallSpec (StaticTarget str _) _ _)) =
  showPpr str
foreignName _ =
  error "Dynamic foreign calls not supported!"

toJSVar :: Var -> JSVar
toJSVar v =
  case idDetails v of
    FCallId fc ->
        Foreign (foreignName fc)
    _ | isExternalName name ->
        External unique external
      | otherwise ->
        Internal unique
      
  where
    name     = P.varName v
    unique   = showPpr $ nameUnique name
    external = showPpr name

-- | Generate a new variable and add a dependency on it to the function
--   currently being generated.
genVar :: Var -> JSGen JSVar
genVar var = do
  let var' = toJSVar var
  dependOn var'
  return var'
