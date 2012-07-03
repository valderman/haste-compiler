{-# LANGUAGE PatternGuards #-}
module CodeGen.Javascript.StgGen (generate) where
import Prelude hiding (catch)
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
import CodeGen.Javascript.Optimize
import CodeGen.Javascript.Errors
import CodeGen.Javascript.Config
import Data.Int
import Data.Word
import Data.Maybe (isJust)
import Control.Exception hiding (evaluate)

generate :: Config -> ModuleName -> [StgBinding] -> JSMod
generate cfg modname binds =
  JSMod {
      AST.name = modname,
      AST.deps = foldl' insDep M.empty theMod,
      AST.code = foldl' insFun M.empty theMod
    }
  where
    theMod = genAST cfg modname binds

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
genAST :: Config -> ModuleName -> [StgBinding] -> [(S.Set JSVar, JSStmt)]
genAST cfg modname binds =
  binds'
  where
    binds' =
      map (depsAndCode . genJS cfg myModName . uncurry (genBind False True))
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
--   If the 'careful' parameter is True, the binding must take care that any
--   closed-over vars are kept constant.
--   Use `genBindRec` to generate code for local potentially recursive bindings 
--   as their dependencies get merged into their parent's anyway.
genBind :: Bool -> Bool -> Maybe Int -> StgBinding -> JSGen Config ()
genBind careful onTopLevel funsInRecGroup (StgNonRec v rhs) = do
  pushBinding v (argsOf rhs)
  v' <- genVar v
  when (not onTopLevel) $ do
    addLocal v'
  let shouldTailLoop = maybe 1 id funsInRecGroup <= 1 && tailRecursive v rhs
  expr <- genRhs careful shouldTailLoop (isJust funsInRecGroup) rhs
  emit $ NewVar (AST.Var v') expr
  popBinding
  where
    argsOf (StgRhsClosure _ _ _ _ _ args _) = args
    argsOf _                                = []
genBind _ _ _ (StgRec _) =
  error $  "genBind got recursive bindings!"

-- | Returns true if the given expression ever tail recurses on the given var.
tailRecursive :: Var -> StgRhs -> Bool
tailRecursive parent (StgRhsClosure _ _ _ _ _ _ body) =
  tr body
  where
    tr (StgApp f _) =
      parent == f
    tr (StgLet _ ex) =
      tr ex
    tr (StgLetNoEscape _ _ _ ex) =
      tr ex
    tr (StgSCC _ _ _ ex) =
      tr ex
    tr (StgTick _ _ ex) =
      tr ex
    tr (StgCase _ _ _ _ _ _ alts) =
      or [tr ex | (_, _, _, ex) <- alts]
    tr _ =
      False
tailRecursive _ _ =
  False

genBindRec :: Bool -> StgBinding -> JSGen Config ()
genBindRec careful bs@(StgRec _) =
  mapM_ (genBind careful False (Just len) . snd) bs'
  where
    bs' = unRec bs
    len = length bs'
genBindRec careful b =
  genBind careful False Nothing b

-- | Turn a recursive binding into a list of non-recursive ones, together with
--   information about whether they came from a recursive group or not.
unRec :: StgBinding -> [(Maybe Int, StgBinding)]
unRec (StgRec bs) = zip (repeat len) (map (uncurry StgNonRec) bs)
  where
    len = Just $ length bs
unRec b           = [(Nothing, b)]

-- | Generate the RHS of a binding. Careful bindings don't close over any
--   mutable vars.
genRhs :: Bool -> Bool -> Bool -> StgRhs -> JSGen Config JSExp
genRhs _ _ recursive (StgRhsCon _ con args) = do
  -- Constructors are never partially applied, and we have arguments, so this
  -- is obviously a full application.
  if recursive
     then Thunk [] <$> genEx False (StgConApp con args)
     else genEx False (StgConApp con args)
genRhs careful tailpos _ (StgRhsClosure _ _ closureDeps upd _ args body) = do
  args' <- mapM genVar args
  (retExp, body') <- isolate $ do
    mapM_ addLocal args'
    genEx tailpos body
  -- Constant-close over all dependencies
  deps <- mapM genVar closureDeps
  constify deps $
    if isUpdatable upd && null args
      then thunk (bagToList body') retExp
      else Fun args' (loop $ bagToList $ body' `snocBag` Ret retExp)
  where
    thunk [] l@(Lit _) = l
    thunk stmts expr   = Thunk stmts expr
    loop | tailpos   = (:[]) . While (litN 1) . Block
         | otherwise = id
    constify deps fun
      | careful =
        return $ ConstClosure deps fun
      | otherwise =
        return fun

-- | Generate the tag for a data constructor. This is used both by genDataCon
--   and directly by genCase to generate constructors for matching.
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

-- | Generate code for data constructor creation.
genDataCon :: DataCon -> JSGen Config JSExp
genDataCon dc = do
  case genDataConTag dc of
    Right t@(Lit (Boolean _)) ->
      return t
    Right t ->
      return $ DataCon t (map strict (dataConRepStrictness dc))
    Left var ->
      return $ AST.Var $ JSVar {jsmod = moduleNameString$AST.name$foreignModule,
                                jsname = Foreign var}
  where
    strict MarkedStrict = True
    strict _            = False

-- | Generate code for an STG expression.  
genEx :: Bool -> StgExpr -> JSGen Config JSExp
genEx tailpos (StgApp f xs) = do
  parent <- getCurrentBinding
  if tailpos && f == parent
    then do
      let mkVar v = genVar v >>= return . AST.Var
      as <- getCurrentBindingArgs >>= mapM mkVar
      bs <- mapM genArg xs
      let assign = zipWith (\l r -> ExpStmt $ Assign l r) as bs
      mapM_ emit assign
      emit Continue
      return $ runtimeError "Unreachable!"
    else do
      f' <- genVar f
      xs' <- mapM genArg xs
      performTCE <- doTCE <$> getCfg
      let nonNullaryFunApp =
            optApp (arityInfo $ idInfo f) $ Call (AST.Var f') xs'
      case null xs of
        True  | performTCE -> return $ AST.Var f'
              | otherwise  -> return $ Eval (AST.Var f')
        False | performTCE -> return $ Thunk [] nonNullaryFunApp
              | otherwise  -> return nonNullaryFunApp
genEx _ (StgLit l) = do
  genLit l
genEx _ (StgConApp con args) = do
  con' <- genDataCon con
  args' <- mapM genArg args
  case con' of
    DataCon tag stricts -> do
      return $ Array (tag : zipWith evaluate stricts args')
    AST.Var v | jsname v == Foreign "I" && 
                jsmod v == moduleNameString (AST.name foreignModule) -> do
      return $ NativeCall "I" args'
    constr@(Lit _) | null args' ->
      return constr
    _ ->
      error $ "Data constructor generated crazy code: " ++ show con'
  where
    evaluate True arg = Eval arg
    evaluate _ arg    = arg
genEx _ (StgOpApp op args _type) = do
  args' <- mapM genArg args
  cfg <- getCfg
  let theOp = case op of
        StgPrimOp op' ->
          genOp cfg op' args'
        StgPrimCallOp (PrimCall f _) ->
          Right $ NativeCall (unpackFS f) args'
        StgFCallOp (CCall (CCallSpec (StaticTarget f _) _ _)) _t ->
          Right $ NativeCall (unpackFS f) args'
        _ ->
          Left "Unsupported primop encountered!"
  case theOp of
    Right x -> return x
    Left e  -> warn Normal e >> return (runtimeError e)
genEx tailpos (StgLet bind expr) = do
  genBindRec tailpos bind
  genEx tailpos expr
genEx tailpos (StgLetNoEscape _ _ bind expr) = do
  genBindRec tailpos bind
  genEx tailpos expr
genEx tailpos (StgCase ex _ _ bndr _ t alts) = do
  genCase tailpos ex bndr t alts
genEx tailpos (StgSCC _ _ _ ex) = do
  genEx tailpos ex
genEx tailpos (StgTick _ _ ex) = do
  genEx tailpos ex
genEx _ (StgLam _ _ _) =
  error "StgLam shouldn't happen during CG!"

genCase :: Bool -> StgExpr -> Id -> AltType -> [StgAlt] -> JSGen Config JSExp
genCase tailpos ex scrut t alts = do
  ex' <- genEx False ex
  scrut' <- genVar scrut
  res <- genResultVar scrut
  addLocal [scrut', res]
  performTCE <- doTCE <$> getCfg
  let expr = if performTCE
               then (Eval ex')
               else ex'
  emit $ NewVar (AST.Var scrut') expr
  genAlts tailpos t scrut' res alts

genAlts :: Bool -> AltType -> JSVar -> JSVar -> [StgAlt] -> JSGen Config JSExp
genAlts tailpos _ scrut res [alt] = do
  altStmts <- getStmts <$> genAlt tailpos scrut res alt
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
genAlts tailpos t scrut res alts = do
  alts' <- mapM (genAlt tailpos scrut res) alts
  optCase cmp scrut res alts'
  where
    getTag = \s -> Index s (litN 0)
    -- We want all our booleans unpacked!
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


genAlt :: Bool -> JSVar -> JSVar -> StgAlt -> JSGen Config JSAlt
genAlt tailpos scrut res (con, args, used, body) = do
  construct <- case con of
    DEFAULT                                  -> return Def
    LitAlt l                                 -> Cond <$> genLit l
    DataAlt c | Right tag <- genDataConTag c -> return $ Cond tag
    _ -> error "Bad data constructor tag generated!"

  args' <- mapM genVar args
  addLocal args'
  let binds = [bindVar v ix | (v, ix, True) <- zip3 args' [1..] used]
  (ret, body') <- isolate $ genEx tailpos body
  return $ construct
         $ bagToList
         $ listToBag binds `unionBags` body' `snocBag` NewVar (AST.Var res) ret
  where
    bindVar var ix = NewVar (AST.Var var) (Index (AST.Var scrut) (litN ix))

genArg :: StgArg -> JSGen Config JSExp
genArg (StgVarArg v)  = AST.Var <$> genVar v
genArg (StgLitArg l)  = genLit l
genArg (StgTypeArg _) = error "Type argument remained in STG!"

genLit :: Literal -> JSGen Config JSExp
genLit l = do
  case l of
    MachStr s           -> return . lit  $ unpackFS s
    MachInt n
      | n > 2147483647 ||
        n < -2147483648 -> do warn Verbose (constFail "Int" n)
                              return $ truncInt n
      | otherwise       -> return . litN $ fromIntegral n
    MachFloat f         -> return . litN $ fromRational f
    MachDouble d        -> return . litN $ fromRational d
    MachChar c          -> return $ lit c
    MachWord w          
      | w > 0xffffffff  -> do warn Verbose (constFail "Word" w)
                              return $ truncWord w
      | otherwise       -> return . litN $ fromIntegral w
    MachWord64 w        -> return . litN $ fromIntegral w
    MachNullAddr        -> return $ litN 0
    MachInt64 n         -> return . litN $ fromIntegral n
    LitInteger _ n      -> AST.Var <$> genVar n
    MachLabel _ _ _     -> return $ lit ":(" -- Labels point to machine code - ignore!
  where
    constFail t n = t ++ " literal " ++ show n ++ " doesn't fit in 32 bits;"
                    ++ " truncating!"
    truncInt n  = litN . fromIntegral $ (fromIntegral n :: Int32)
    truncWord w = litN . fromIntegral $ (fromIntegral w :: Word32)

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
genVar :: Var -> JSGen Config JSVar
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
genResultVar :: Var -> JSGen Config JSVar
genResultVar var = do
  thisMod <- getModName
  return $! toJSVar thisMod var (Just "::|RESULT|")
