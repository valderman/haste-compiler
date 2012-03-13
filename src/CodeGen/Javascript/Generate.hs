{-# LANGUAGE PatternGuards #-}
-- | Module to generate Javascript from a ModGuts structure
module CodeGen.Javascript.Generate (generate) where
import GhcPlugins as P
import ForeignCall
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (foldl')

import CodeGen.Javascript.Monad
import CodeGen.Javascript.AST as AST hiding (unique,name,deps,code)
import qualified CodeGen.Javascript.AST as AST (name, deps, code)
import Bag
import CodeGen.Javascript.PrimOps
import CodeGen.Javascript.PrintJS (prettyJS, pseudo)

-- | Turn a pile of Core into our intermediate JS AST.
generate :: CgGuts -> JSMod
generate guts =
  JSMod {
      AST.name = moduleName $ cg_module guts,
      AST.deps = foldl' insDep M.empty theMod,
      AST.code = foldl' insFun M.empty theMod
    }
  where
    -- With the switch to CgGuts, we often get code like:
    -- foo = \a b -> ...
    -- foo = foo
    -- Just filtering these assignments out seems to fix the issue, but I'm not
    -- quite sure why we get this.
    theMod = filter notSelfAssign (genAST guts)

    notSelfAssign (_, NewVar (AST.Var v) (AST.Var v')) =
      not (v == v')
    notSelfAssign _ =
      True

    insFun m (_, NewVar (AST.Var name) fun) =
      M.insert name fun m
    insFun _ ex =
      error $ "Non-NewVar top binding to insFun: " ++ show ex
    
    insDep m (deps, NewVar (AST.Var name) _) =
      M.insert name (S.delete name deps) m
    insDep _ ex =
      error $ "Non-NewVar top binding to insDep: " ++ show ex

-- | Generate JS AST for bindings and class method stubs.
--   The method stubs are just functions that take a class dictionary as their
--   sole inputs, and return the appropriate function from that dictionary.
--   For instance, the stub for the second method of a class C would look like
--   this:
--     function C.secondMethod(dict) {
--       return dict[2];
--     }
genAST :: CgGuts -> [(S.Set JSVar, JSStmt)]
genAST guts =
  binds
  where
    binds =
      map (depsAndCode . genJS . genBind) . concatMap unRec $ cg_binds guts

    depsAndCode (_, deps, code) =
      case bagToList code of
        [code'] ->
          (deps, code')
        lotsOfCode ->
          error $  "Single top level exp generated several assignments!\n"
                ++ prettyJS pseudo lotsOfCode

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
genThunk ex
  | isUnliftedExp ex = do
    genEx ex
  | isVar ex = do
    genEx ex
  | isFunTy . dropForAlls $ exprType ex = do
    genEx ex
  | isTyVarTy $ exprType ex = do
    genEx ex
  | exprIsHNF ex = do
    genEx ex
  | otherwise = do
    let (ex', deps, supportStmts) = genJS $ genEx ex
    dependOn deps
    return $ Thunk (bagToList supportStmts) ex'

-- | Generate an eval expression, where needed. Unlifted types don't need it.
genEval :: Expr Var -> JSGen JSExp
genEval ex
  | isUnliftedExp ex = do
    genEx ex
  | otherwise = do
    genEx ex >>= return . Eval 

genEx :: Expr Var -> JSGen JSExp
genEx (P.Var v) = do
  case idDetails v of
    DataConWorkId dc -> genDataCon dc
    DataConWrapId dc -> genDataCon dc
    _                -> genVar v >>= return . AST.Var
genEx (P.Lit l) =
  genLit l
genEx (App f arg) = do
  genApp f arg >>= return . foldUpApp
genEx (Lam v ex) =
  genFun [v] ex >>= return . foldUpFun
genEx (Let bind ex) = do
  genBindRec bind
  genEx ex
genEx (P.Case ex v t alts) =
  genCase ex v t alts
genEx (Cast ex _) =
  genEx ex
genEx (Tick _ ex) =
  genEx ex
genEx (Type _) =
  error "Type annotation encountered where it shouldn't be!"
genEx (Coercion _) =
  error "Don't know what to do with a coercion!"

-- | Generate code for funcion application
genApp :: Expr Var -> Arg Var -> JSGen JSExp
genApp expr (Type _) = do
  -- Discard type annotations
  genEx expr
genApp expr arg = do 
  arg' <- genThunk arg
  poExp <- genPrimOp expr [arg']
  case poExp of
    Just primop -> do
      return primop
    _ -> do
      expr' <- genEx expr
      return $ Call expr' [arg']

-- | Fold up nested function applications into a single call as far as
--   possible.
foldUpApp :: JSExp -> JSExp
foldUpApp (Call f as) =
  case foldUpApp f of
    Call f' as' -> Call f' (as' ++ as)
    f'          -> Call f' as
foldUpApp expr =
  expr

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
genPrimOp (P.Var v) xs | PrimOpId op <- idDetails v =
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
genLit l = do
  return $ AST.Lit $ case l of
    MachStr s      -> Str $ show s
    MachInt n      -> Num $ fromIntegral n
    MachFloat f    -> Num $ fromRational f
    MachDouble d   -> Num $ fromRational d
    MachChar c     -> Chr c
    MachWord w     -> Num $ fromIntegral w
    MachWord64 w   -> Num $ fromIntegral w
    MachNullAddr   -> Num 0
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
foldUpFun expr =
  expr

genCase :: Expr Var -> Var -> Type -> [Alt P.Var] -> JSGen JSExp
genCase expr v _ alts = do
  v' <- genVar v
  expr' <- genEval expr
  emit $ NewVar (AST.Var v') expr'
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
  as' <- mapM (genAlt v) as
  case as' of
    (a''@(Def _)):as'' -> emit $ AST.Case (AST.Var v) $ as'' ++ [a'']
    _                  -> emit $ AST.Case (AST.Var v) as'
  return $ AST.Var v

-- | Generate a case alternative. Each alternative is responsible for binding
--   any constructor fields and generating any code needed to produce the alt's
--   result. That result is then bound to the AST.Var given.
--   Special care is taken to make sure any evaluation performed within the
--   alternative's expression is actually confined to the alternative's
--   generated code.
genAlt :: JSVar -> Alt P.Var -> JSGen JSAlt
genAlt resultVar (con, binds, expr) = do
  con' <- case con of
    DEFAULT   -> return Def
    LitAlt l  -> genLit l >>= return . Cond
    DataAlt c -> return . Cons $ dataConTag c
  let (retEx, deps, body) = genJS (genBinds binds >> genEx expr)
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
    _
      | isLocalId v && not (isExportedId v) ->
        Internal unique
      | otherwise ->
        External external
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
