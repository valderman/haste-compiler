{-# LANGUAGE PatternGuards #-}
module CodeGen.Javascript.StgGen (generate) where
import BasicTypes
import StgSyn
import CoreSyn (AltCon (..))
import DataCon
import Name
import Module
import Bag
import Data.List
import Literal
import FastString
import Var
import ForeignCall
import PrimOp
import IdInfo
import Outputable
import TyCon
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import CodeGen.Javascript.AST as AST hiding (unique, name, deps, code)
import qualified CodeGen.Javascript.AST as AST (name, deps, code)
import CodeGen.Javascript.Monad
import CodeGen.Javascript.PrintJS
import CodeGen.Javascript.Builtins
import CodeGen.Javascript.PrimOps

generate :: ModuleName -> [StgBinding] -> JSMod
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
genAST :: ModuleName -> [StgBinding] -> [(S.Set JSVar, JSStmt)]
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

-- | Generate code for all bindings. genBind spits out an error if it receives
--   a recursive binding; this is because it's quite a lot easier to keep track
--   of which functions depend on each other if every genBind call results in a
--   single function being generated.
--   Use `genBindRec` to generate code for local potentially recursive bindings 
--   as their dependencies get merged into their parent's anyway.
genBind :: Bool -> StgBinding -> JSGen ()
genBind onTopLevel (StgNonRec v rhs) = do
  pushBinding v
  v' <- genVar v
  when (not onTopLevel) (addLocal v')
  expr <- genRhs rhs
  emit $ NewVar (AST.Var v') expr
  popBinding
genBind _ (StgRec _) =
  error $  "genBind got recursive bindings!"

genBindRec :: StgBinding -> JSGen ()
genBindRec bs@(StgRec _) =
  mapM_ (genBind False) (unRec bs)
genBindRec b =
  genBind False b

-- | Turn a recursive binding into a list of non-recursive ones.
unRec :: StgBinding -> [StgBinding]
unRec (StgRec bs) = map (uncurry StgNonRec) bs
unRec b           = [b]

-- | Generate the RHS of a binding.
genRhs :: StgRhs -> JSGen JSExp
genRhs (StgRhsCon _ con args) = do
  if length args == dataConRepArity con
     then genEx (StgConApp con args)
     else genDataCon con
genRhs (StgRhsClosure _ _ _ upd _ args body) = do
  args' <- mapM genVar args
  (retExp, body', _) <- isolate $ genEx body
  if isUpdatable upd && null args
     then return $ thunk (bagToList body') retExp
     else return $ Fun args' (bagToList $ body' `snocBag` Ret retExp)
  where
    thunk [] l@(Lit _) = l
    thunk stmts expr   = Thunk stmts expr

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
    _                          -> Right $ lit (fromIntegral $ dataConTag d :: Double)

-- | Generate code for the given data constructor.
genDataCon :: DataCon -> JSGen JSExp
genDataCon dc = do
  case genDataConTag dc of
    Right t ->
      return $ DataCon t (map strict (dataConRepStrictness dc))
    Left var ->
      return $ AST.Var $ JSVar {jsmod = moduleNameString$AST.name$foreignModule,
                                jsname = Foreign var}
  where
    strict MarkedStrict = True
    strict _            = False

-- | Generate code for an STG expression.  
genEx :: StgExpr -> JSGen JSExp
genEx (StgApp f xs) = do
  f' <- genVar f
  xs' <- mapM genArg xs
  if null xs
     then return $ Eval $ AST.Var f'
     else return $ Call (AST.Var f') xs'
genEx (StgLit l) = do
  genLit l
genEx (StgConApp con args) = do
  con' <- genDataCon con
  args' <- mapM genArg args
  case con' of
    DataCon tag@(Lit (Boolean _)) [] -> do
      return tag
    DataCon tag stricts -> do
      return $ Array (tag : zipWith evaluate stricts args')
    AST.Var v | jsname v == Foreign "I" && 
                jsmod v == moduleNameString (AST.name foreignModule) -> do
      return $ NativeCall "I" args'
    _ ->
      error $ "Data constructor generated crazy code: " ++ show con'
  where
    evaluate True arg = Eval arg
    evaluate _ arg    = arg
genEx (StgOpApp op args _type) = do
  args' <- mapM genArg args
  return $ case op of
    StgPrimOp op' ->
      genOp op' args'
    StgPrimCallOp (PrimCall f _) ->
      NativeCall (unpackFS f) args'
    StgFCallOp (CCall (CCallSpec (StaticTarget f _) _ _)) _t ->
      NativeCall (unpackFS f) args'
    _ ->
      error "Unsupported primop encountered!"
genEx (StgLet bind expr) = do
  genBindRec bind
  genEx expr
genEx (StgLetNoEscape _ _ bind expr) = do
  genBindRec bind
  genEx expr
genEx (StgCase ex _ _ bndr _ t alts) = do
  genCase ex bndr t alts
genEx (StgSCC _ _ _ ex) = do
  genEx ex
genEx (StgTick _ _ ex) = do
  genEx ex
genEx (StgLam _ _ _) =
  error "StgLam shouldn't happen during CG!"

genCase :: StgExpr -> Id -> AltType -> [StgAlt] -> JSGen JSExp
genCase ex scrut t alts = do
  ex' <- genEx ex
  scrut' <- genVar scrut
  res <- genResultVar scrut
  emit $ NewVar (AST.Var scrut') ex'
  genAlts t scrut' res alts

genAlts :: AltType -> JSVar -> JSVar -> [StgAlt] -> JSGen JSExp
genAlts _ scrut res [alt] = do
  altStmts <- getStmts <$> genAlt scrut res alt
  -- As this alternative is the only one in this case expression, it's OK to
  -- eliminate var -> var assigns at the end of it.
  case last altStmts of
    NewVar lhs rhs@(AST.Var _) | lhs == AST.Var res -> do
      mapM_ emit (init altStmts)
      return rhs
    _ -> do
      mapM_ emit altStmts
      return $ AST.Var res
  where
    getStmts (Cond _ ss) = ss
    getStmts (Def ss)    = ss
genAlts t scrut res alts = do
  alts' <- mapM (genAlt scrut res) alts
  case simplifyAlts cmp scrut alts' of
    Left stmt -> do
      case ifToTernary stmt of
        Just tExp -> return tExp
        _         -> emit stmt >> return (AST.Var res)
    Right alts'' -> do
      emit $ Case (cmp $ AST.Var scrut) alts''
      return $ AST.Var res
  where
    getTag = \s -> Index s (litN 0)
    cmp = case t of
      PrimAlt _ -> id
      AlgAlt tc -> if tyConIsBoolean tc then id else getTag
      _         -> getTag

tyConIsBoolean :: TyCon -> Bool
tyConIsBoolean tc =
  case (n, m) of
    ("Bool", "GHC.Types")  -> True
    _                      -> False
  where
    n = occNameString $ nameOccName $ tyConName tc
    m = moduleNameString $ moduleName $ nameModule $ tyConName tc


genAlt :: JSVar -> JSVar -> StgAlt -> JSGen JSAlt
genAlt scrut res (con, args, used, body) = do
  construct <- case con of
    DEFAULT                                  -> return Def
    LitAlt l                                 -> Cond <$> genLit l
    DataAlt c | Right tag <- genDataConTag c -> return $ Cond tag
    _ -> error "Bad data constructor tag generated!"

  args' <- mapM genVar args
  let binds = [bindVar v ix | (v, ix, True) <- zip3 args' [1..] used]
  (ret, body', _) <- isolate $ genEx body
  return $ construct
         $ bagToList
         $ listToBag binds `unionBags` body' `snocBag` NewVar (AST.Var res) ret
  where
    bindVar var ix = NewVar (AST.Var var) (Index (AST.Var scrut) (litN ix))


-- | Turn if statements with single expression branches into expressions using
--   the ternary operator.
ifToTernary :: JSStmt -> Maybe JSExp
ifToTernary (If cond thenDo elseDo) = do
  then' <- getOkExpr thenDo
  else' <- getOkExpr elseDo
  return $ IfExp cond then' else'
  where
    getOkExpr [NewVar _ ex] = Just ex
    getOkExpr _             = Nothing
ifToTernary _ =
  Nothing

-- | Turn a few common switch statements into smaller code constructs.
--   These transformations rely on the fact that a Core case expression must
--   always be complete and that its alternatives may not overlap.
--   This means that if we have two alternatives, if the first one does not
--   match, the second one will.
simplifyAlts :: (JSExp -> JSExp) -> JSVar -> [JSAlt] -> Either JSStmt [JSAlt]
simplifyAlts cmp scrut as =
  case as of
    -- Core requires DEFAULT alts to come first, but we want them last in JS.
    (a'@(Def _):as'@(_:_)) ->
      simplifyAlts cmp scrut (as' ++ [a'])

    -- Turn any false/anything comparisons into if statements.
    -- As case alts are ordered ascending, false will always come first.
    [Cond (AST.Lit (Boolean False)) ifFalseDo, ifTrueDo] ->
      Left $ If (cmp (AST.Var scrut)) (getStmts ifTrueDo) ifFalseDo
    [Cond (AST.Lit (Num 0)) ifFalseDo, ifTrueDo] ->
      Left $ If (cmp $ AST.Var scrut) (getStmts ifTrueDo) ifFalseDo
    
    -- Turn any two-alt switch statements into if/then/else.
    [Cond cond thenDo, elseDo] ->
      Left $ If (BinOp Eq (cmp $ AST.Var scrut) cond)
                thenDo
                (getStmts elseDo)

    -- No interesting transformations to make
    _ ->
      Right as
  where
    getStmts (Def ss) = ss
    getStmts (Cond _ ss) = ss

genArg :: StgArg -> JSGen JSExp
genArg (StgVarArg v)  = AST.Var <$> genVar v
genArg (StgLitArg l)  = genLit l
genArg (StgTypeArg _) = error "Type argument remained in STG!"

genLit :: Literal -> JSGen JSExp
genLit l = do
  case l of
    MachStr s       -> return . lit  $ unpackFS s
    MachInt n       -> return . litN $ fromIntegral n
    MachFloat f     -> return . litN $ fromRational f
    MachDouble d    -> return . litN $ fromRational d
    MachChar c      -> return $ lit c
    MachWord w      -> return . litN $ fromIntegral w
    MachWord64 w    -> return . litN $ fromIntegral w
    MachNullAddr    -> return $ litN 0
    MachInt64 n     -> return . litN $ fromIntegral n
    LitInteger _ n  -> AST.Var <$> genVar n
    MachLabel _ _ _ -> return $ lit ":(" -- Labels point to machine code - ignore!

-- | Extracts the name of a foreign var.
foreignName :: ForeignCall -> String
foreignName (CCall (CCallSpec (StaticTarget str _) _ _)) =
  unpackFS str
foreignName _ =
  error "Dynamic foreign calls not supported!"

toJSVar :: JSLabel -> Var -> Maybe String -> JSVar
toJSVar thisMod v msuffix =
  case idDetails v of
    FCallId fc ->
        JSVar {jsname = Foreign (foreignName fc),
               jsmod  = moduleNameString $ AST.name foreignModule}
    _
      | isLocalId v && not hasMod ->
        JSVar {jsname = Internal $ unique ++ suffix,
               jsmod  = myMod}
      | isGlobalId v || hasMod ->
        JSVar {jsname = External $ extern ++ suffix,
               jsmod  = myMod}
      | otherwise ->
          error $ "Var is neither foreign, local or global: " ++ show v
  where
    suffix = case msuffix of
               Just s -> s
               _      -> ""
    name   = Var.varName v
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
      let var' = toJSVar thisMod var Nothing
      dependOn var'
      return $! var'

-- | Generate a result variable for the given scrutinee variable.
--   Each scrutinee has exactly one result variable; previously, we used the
--   scrutinee as the result, but that was a horrible idea as that meant lazy
--   code accidentally used scrutinees that had already been overwritten with
--   results, so we need a new one.
--   By keeping strictly to SSA, we ensure that this sort of lazy sharing is
--   perfectly OK.
genResultVar :: Var -> JSGen JSVar
genResultVar var = do
  thisMod <- getModName
  return $! toJSVar thisMod var (Just "::|RESULT|")
