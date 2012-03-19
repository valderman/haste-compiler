{-# LANGUAGE PatternGuards #-}
-- | Module to generate Javascript from a ModGuts structure
module CodeGen.Javascript.Generate (generate) where
import GhcPlugins as P
import ForeignCall
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (foldl')
import Bag
import Control.Monad
import Control.Applicative
import CodeGen.Javascript.Monad
import CodeGen.Javascript.AST as AST hiding (unique,name,deps,code)
import qualified CodeGen.Javascript.AST as AST (name, deps, code)
import CodeGen.Javascript.PrimOps
import CodeGen.Javascript.Builtins
import CodeGen.Javascript.PrintJS (prettyJS, pseudo)

-- | Turn a pile of Core into our intermediate JS AST.
generate :: ModuleName -> CoreProgram -> JSMod
generate modname binds =
  JSMod {
      AST.name = modname,
      AST.deps = foldl' insDep M.empty theMod,
      AST.code = foldl' insFun M.empty theMod
    }
  where
    theMod = genAST modname binds

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
genAST :: ModuleName -> CoreProgram -> [(S.Set JSVar, JSStmt)]
genAST modname binds =
  binds'
  where
    binds' =
      map (depsAndCode . genJS myModName . genBind True)
      $ concatMap unRec
      $ binds

    myModName = moduleNameString modname

    depsAndCode (_, deps, locs, code) =
      case bagToList code of
        [code'] ->
          (deps S.\\ locs, code')
        lotsOfCode ->
          case last lotsOfCode of
            NewVar v ex ->
              (deps, NewVar v $ Fun [] (init lotsOfCode ++ [Ret ex]))
            _ ->
              error $ "Totally weird code generated for symbol: " ++
                      fst (prettyJS pseudo bogusJSVar lotsOfCode)

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
genBind :: Bool -> CoreBind -> JSGen ()
genBind onTopLevel (NonRec v ex) = do
  pushBinding v
  v' <- genVar v
  when (not onTopLevel) (addLocal v')
  ex' <- genThunk ex
  emit $ NewVar (AST.Var v') ex'
  popBinding
genBind _ (Rec bs) =
  error $  "genBind got recursive bindings: "
        ++ showPpr bs

-- | Generate code for a potentially recursive binding. Only use this for local
--   functions; use `genBind` for top level bindings instead.
genBindRec :: CoreBind -> JSGen ()
genBindRec bs@(Rec _) = mapM_ (genBind False) (unRec bs)
genBindRec b          = genBind False b

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
  | otherwise = do
    needsThunk <- exprNeedsThunk ex
    if needsThunk
      then do
        (ex', supportStmts, _) <- isolate $ genEx ex
        return $ Thunk (bagToList supportStmts) ex'
      else do
        genEx ex

-- | Returns whether the expression needs to be thunked or not.
--   We don't want to generate thunks for HNF expressions if we can avoid it,
--   but if an expression refers to itself outside the body of a thunk or a
--   lambda, we have to.
exprNeedsThunk :: Expr Var -> JSGen Bool
exprNeedsThunk expr
  | exprIsHNF expr = do
    myMod <- getModName
    var <- toJSVar myMod <$> getCurrentBinding
    return $ check myMod var expr
  | otherwise =
    return True
    where
      check m v (P.Var v')   = v == toJSVar m v'
      check m v (App f a)    = check m v f || check m v a
      check _ _ (Lam _ _)    = False
      check _ _ (P.Lit _)    = False
      check m v (Let _ ex)   = check m v ex
      check m v (Cast ex _)  = check m v ex
      check m v (Tick _ ex)  = check m v ex
      check _ _ (Type _)     = False
      check _ _ (Coercion _) = False
      check _ _ ex           = error $ "Non-HNF expr said to be HNF!"++showPpr ex
      -- Case will never occur in a HNF expression

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
  genApp f arg >>= return . tryFastCall f . tryShrinkConstrs . foldUpApp
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
  return NoOp

-- | Turn saturated application of constructors without strict fields into
--   constant values.
tryShrinkConstrs :: JSExp -> JSExp
tryShrinkConstrs (Call (DataCon tag stricts) as)
  | length stricts == length as =
    Array (tag : zipWith strictify as stricts)
  where
    strictify val False = val
    strictify val _     = Eval val
tryShrinkConstrs app =
  app

-- | Try to perform a fast call; that is, getting rid of the expensive
--   eval/apply machinery.
tryFastCall :: Expr Var -> JSExp -> JSExp
tryFastCall (P.Var var) (Call f as)
  | arityInfo (idInfo var) == length as =
    FastCall f as
tryFastCall _ app =
  app

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

-- | Generate the tag for a data constructor. This lives in its own function
--   as we want to generate true/false for True/False, a function call for
--   Integer literals, and "real" tags for everything else.
--
--   IMPORTANT: remember to update the RTS if any changes are made to the
--              constructor tag values!
genDataConTag :: DataCon -> Either JSLabel JSExp
genDataConTag d = do
  let n = occNameString $ nameOccName $ dataConName d
      m = moduleNameString $ moduleName $ nameModule $ dataConName d
  case (n, m) of
    ("True", "GHC.Types")      -> Right $ lit True
    ("False", "GHC.Types")     -> Right $ lit False
    ("S#", "GHC.Integer.Type") -> Left "I"
    _       -> Right $ lit $ (fromIntegral $ dataConTag d :: Double)

-- | Generate code for the given data constructor
genDataCon :: DataCon -> JSGen JSExp
genDataCon dc = do
  case genDataConTag dc of
    Right t ->
      return $ DataCon t (map strict (dataConRepStrictness dc))
    Left var ->
      return $ AST.Var $ JSVar {jsmod=moduleNameString$AST.name$foreignModule,
                                jsname = Foreign var}
  where
    strict MarkedStrict = True
    strict _            = False

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
  case l of
    MachStr s       -> return . lit $ show s
    MachInt n       -> return . litN $ fromIntegral n
    MachFloat f     -> return . litN $ fromRational f
    MachDouble d    -> return . litN $ fromRational d
    MachChar c      -> return $ lit c
    MachWord w      -> return . litN $ fromIntegral w
    MachWord64 w    -> return . litN $ fromIntegral w
    MachNullAddr    -> return $ litN 0
    MachInt64 n     -> return . litN $ fromIntegral n
    LitInteger _ n  -> AST.Var <$> genVar n
    MachLabel _ _ _ -> return $ litN 0 -- Labels point to machine code - ignore!

-- | Generate code for a lambda and return it. Care is taken to ensure any and
--   all evaluation takes place within the function where it's actually
--   supposed to happen.
genFun :: [Var] -> Expr Var -> JSGen JSExp
genFun [v] body | isTyVar v =
  genEx body
genFun vs body = do
  vs' <- mapM genVar vs
  (retExp, body', _) <- isolate $ genEx body
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
  altStmts <- getStmts <$> genAlt v a
  -- As this alternative is the only one in this case expression, it's OK to
  -- eliminate var -> var assigns at the end of it.
  case last altStmts of
    ExpStmt (Assign lhs rhs@(AST.Var _)) | lhs == AST.Var v -> do
      mapM_ emit (init altStmts)
      return rhs
    _ -> do
      mapM_ emit altStmts
      return $ AST.Var v
  where
    getStmts (Cond _ ss) = ss
    getStmts (Def ss)    = ss
genAlts v as = do
  as' <- mapM (genAlt v) as
  case simplifyAlts v as' of
    Left stmt -> do
      case ifToTernary stmt of
        Just tExp -> return tExp
        _         -> emit stmt >> return (AST.Var v)
    Right alts -> do
      emit $ AST.Case (AST.Var v) alts
      return $ AST.Var v

-- | Turn if statements with single expression branches into expressions using
--   the ternary operator.
ifToTernary :: JSStmt -> Maybe JSExp
ifToTernary (If cond thenDo elseDo) = do
  then' <- getOkExpr thenDo
  else' <- getOkExpr elseDo
  return $ IfExp cond then' else'
  where
    getOkExpr [ExpStmt (Assign _ ex)] = Just ex
    getOkExpr _                       = Nothing
ifToTernary _ =
  Nothing

-- | Turn a few common switch statements into smaller code constructs.
--   These transformations rely on the fact that a Core case expression must
--   always be complete and that its alternatives may not overlap.
--   This means that if we have two alternatives, if the first one does not
--   match, the second one will.
simplifyAlts :: JSVar -> [JSAlt] -> Either JSStmt [JSAlt]
simplifyAlts v as =
  case as of
    -- Core requires DEFAULT alts to come first, but we want them last in JS.
    (a'@(Def _):as'@(_:_)) ->
      simplifyAlts v (as' ++ [a'])

    -- Turn any false/anything comparisons into if statements.
    -- As case alts are ordered ascending, false will always come first.
    [Cond (AST.Lit (Boolean False)) ifFalseDo, ifTrueDo] ->
      Left $ If (AST.Var v) (getStmts ifTrueDo) ifFalseDo
    [Cond (AST.Lit (Num 0)) ifFalseDo, ifTrueDo] ->
      Left $ If (AST.Var v) (getStmts ifTrueDo) ifFalseDo
    
    -- Turn any two-alt switch statements into if/then/else.
    [Cond cond thenDo, elseDo] ->
      Left $ If (BinOp Eq (AST.Var v) cond) thenDo (getStmts elseDo)

    -- No interesting transformations to make
    _ ->
      Right as
  where
    getStmts (Def ss) = ss
    getStmts (Cond _ ss) = ss

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
    DataAlt c -> case genDataConTag c of 
      Right tag -> return $ Cond tag
      _         -> error "Integer literal in case alt!"
  (retEx, body, touched) <- isolate $ genEx expr
  (_, binds', _) <- isolate $ genBinds touched binds
  return
    . con'
    . bagToList
    $ binds' `unionBags`
      -- NOTE: remember to update ifToTernary if this last line changes!
      body `snocBag` (ExpStmt $ Assign (AST.Var resultVar) retEx)
  where    
    -- Generate variables for all data constructor arguments, then bind the
    -- actual arguments to them. Only call wrapped in isolate or these bindings
    -- will end up outside its respective case alternative, likely crashing the
    -- program.
    genBinds touched =
      sequence_ . zipWith (genArgBind touched) [1..] . filter (not . isTyVar)

    -- Only generate bindings for vars marked as touched; the others will never
    -- be used anyway.
    genArgBind touched num var = do
      var' <- genVar var
      when (var' `S.member` touched) $ do
        emit $ (NewVar (AST.Var var') (GetDataArg (AST.Var resultVar) num))

-- | Extracts the name of a foreign var.
foreignName :: ForeignCall -> String
foreignName (CCall (CCallSpec (StaticTarget str _) _ _)) =
  showPpr str
foreignName _ =
  error "Dynamic foreign calls not supported!"

toJSVar :: JSLabel -> Var -> JSVar
toJSVar thisMod v =
  case idDetails v of
    FCallId fc ->
        JSVar {
            jsname = Foreign (foreignName fc),
            jsmod  = moduleNameString $ AST.name foreignModule
          }
    _
      | isLocalId v && not hasMod ->
        JSVar {
            jsname = Internal unique,
            jsmod  = myMod
          }
      | isGlobalId v || hasMod ->
        JSVar {
            jsname = External extern,
            jsmod  = myMod
          }
      | otherwise ->
          error $ "Var is neither foreign, local or global: " ++ show v
  where
    name   = P.varName v
    hasMod = case nameModule_maybe name of
               Nothing -> False
               _       -> True
    myMod  =
      maybe thisMod (moduleNameString . moduleName) (nameModule_maybe name)
    extern = occNameString $ nameOccName name
    unique = showPpr $ nameUnique name

-- | Generate a new variable and add a dependency on it to the function
--   currently being generated.
genVar :: Var -> JSGen JSVar
genVar var = do
  thisMod <- getModName
  case toBuiltin var of
    Just var' -> return var'
    _         -> do
      let var' = toJSVar thisMod var
      dependOn var'
      return var'
