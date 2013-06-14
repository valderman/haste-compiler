{-# LANGUAGE PatternGuards, ParallelListComp #-}
module CodeGen.Javascript.StgGen (generate) where
import Prelude
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
import TyCon
import Type
import TysPrim
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
import CodeGen.Javascript.Replace
import CodeGen.Javascript.Traverse
import CodeGen.Javascript.Util
import Data.Int
import Data.Char (ord, chr)
import Data.Word
import Data.Maybe (isJust)

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

-- | Generate JS AST for bindings.
genAST :: Config -> ModuleName -> [StgBinding] -> [(S.Set JSVar, JSStmt)]
genAST cfg modname binds =
  binds'
  where
    binds' =
      map (depsAndCode . genJS cfg myModName . uncurry (genBind True))
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
genBind :: Bool -> Maybe Int -> StgBinding -> JSGen Config ()
genBind onTopLevel funsInRecGroup (StgNonRec v rhs) = do
  pushBinding v (argsOf rhs)
  v' <- genVar v
  when (not onTopLevel) $ do
    addLocal v'
  let shouldTailLoop = maybe 1 id funsInRecGroup <= 1 && tailRecursive v rhs
  expr <- genRhs shouldTailLoop (isJust funsInRecGroup) rhs
  emit $ NewVar (AST.Var v') expr
  popBinding
  where
    argsOf (StgRhsClosure _ _ _ _ _ args _) = filter hasRepresentation args
    argsOf _                                = []
genBind _ _ (StgRec _) =
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

genBindRec :: StgBinding -> JSGen Config ()
genBindRec bs@(StgRec _) =
  mapM_ (genBind False (Just len) . snd) bs'
  where
    bs' = unRec bs
    len = length bs'
genBindRec b =
  genBind False Nothing b

-- | Turn a recursive binding into a list of non-recursive ones, together with
--   information about whether they came from a recursive group or not.
unRec :: StgBinding -> [(Maybe Int, StgBinding)]
unRec (StgRec bs) = zip (repeat len) (map (uncurry StgNonRec) bs)
  where
    len = Just $ length bs
unRec b           = [(Nothing, b)]

-- | Generate the RHS of a binding.
genRhs :: Bool -> Bool -> StgRhs -> JSGen Config JSExp
genRhs _ recursive (StgRhsCon _ con args) = do
  -- Constructors are never partially applied, and we have arguments, so this
  -- is obviously a full application.
  if recursive
     then Thunk [] <$> genEx False (StgConApp con args)
     else genEx False (StgConApp con args)
genRhs tailpos _ (StgRhsClosure _ _ _ upd _ args body) = do
  args' <- mapM genVar args
  (retExp, body') <- isolate $ do
    mapM_ addLocal args'
    genEx tailpos body
  return $ if isUpdatable upd && null args
             then thunk (bagToList body') retExp
             else Fun args' (loop args' $ bagToList $ body' `snocBag` Ret retExp)
  where
    thunk [] l@(Lit _) = l
    thunk stmts expr   = Thunk stmts expr
    
    loop deps | tailpos   = (:[]) . While (litN 1) . Block . maybeCopyVars deps
              | otherwise = id
    
    -- Only copy vars if closures are created in the loop. This frees tight,
    -- unboxed loops from the performance penalty of an extra compare and call
    -- each iteration.
    maybeCopyVars vars fnBody
      | createsClosures (Block fnBody) =
        let locals = localCopies vars
            -- First replace all vars with their local copies, then replace all
            -- locals on the LHS with their external counterparts again.
            -- Slow and stupid, but slightly simpler than doing one smart
            -- replacement pass. Replace ASAP!
            body' = replace (repVarsLHS (zip locals vars))
                  $ replace (repVars (zip vars locals)) fnBody
        in return $ LocalCopy locals vars body'
      | otherwise =
          replace returnNullWithContinue fnBody
    
    returnNullWithContinue = repNothing {
        repStm = \e -> case e of
           Ret Null -> Continue
           _        -> e
      }
    
    -- Does the given code create any closures?
    -- This is a bit too coarse-grained; we could eliminate the local copy for
    -- a few more cases by only counting closures that reference any of the
    -- tail-looped arguments.
    createsClosures code =
      case traverse isClosure False code of
        Just x -> x
        _      -> False
    
    isClosure (Exp (Fun _ _))   _ = done True
    isClosure (Exp (Thunk _ _)) _ = done True
    isClosure _ acc               = return acc
    
-- | Create local copies of the given vars.
localCopies :: [JSVar] -> [JSVar]
localCopies = map mkLocal
  where
    mkLocal x@(JSVar _ (Foreign _)) = x
    mkLocal x                       = fmap (";n_" ++) x

-- | Generate the tag for a data constructor. This is used both by genDataCon
--   and directly by genCase to generate constructors for matching.
--
--   IMPORTANT: remember to update the RTS if any changes are made to the
--              constructor tag values!
genDataConTag :: DataCon -> JSExp
genDataConTag d = do
  let n = occNameString $ nameOccName $ dataConName d
      m = moduleNameString $ moduleName $ nameModule $ dataConName d
  case (n, m) of
    ("True", "GHC.Types")  -> lit True
    ("False", "GHC.Types") -> lit False
    _                      -> lit (fromIntegral $ dataConTag d :: Double)

-- | Generate code for data constructor creation.
genDataCon :: DataCon -> JSGen Config JSExp
genDataCon dc = do
  case genDataConTag dc of
    t@(Lit (Boolean _)) ->
      return t
    t ->
      return $ DataCon t (map strict (dataConRepStrictness dc))
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
      let assign l r = ExpStmt $ Assign l r
      mapM_ emit (zipWith assign as bs)
      emit (Ret Null)
      return $ runtimeError "Unreachable!"
    else do
      f' <- genVar f
      xs' <- mapM genArg xs
      let arity      = arityInfo $ idInfo f
      return $ case null xs of
        True          -> Eval (AST.Var f')
        _ | null xs'  -> Call (AST.Var f') []
          | otherwise -> optApp arity $ Call (AST.Var f') xs'
genEx _ (StgLit l) = do
  genLit l
genEx _ (StgConApp con args) = do
  con' <- genDataCon con
  case con' of
    DataCon tag stricts -> do
      (args', stricts') <- genArgsPair $ zip args stricts
      -- Don't create unboxed tuples with a single element.
      case (isUnboxedTupleCon con, args') of
        (True, [arg]) -> return $ evaluate arg (head stricts')
        _             -> return $ Array (tag : zipWith evaluate args' stricts')
    constr@(Lit _) ->
      return constr
    _ ->
      error $ "Data constructor generated crazy code: " ++ show con'
  where
    evaluate arg True = Eval arg
    evaluate arg _    = arg
genEx _ (StgOpApp op args _type) = do
  args' <- genArgs args
  cfg <- getCfg
  let theOp = case op of
        StgPrimOp op' ->
          genOp cfg op' args'
        StgPrimCallOp (PrimCall f _) ->
          Right $ NativeCall (unpackFS f) args'
        StgFCallOp (CCall (CCallSpec (StaticTarget f _ _) _ _)) _t ->
          Right $ NativeCall (unpackFS f) args'
        _ ->
          Left "Unsupported primop encountered!"
  case theOp of
    Right x -> return x
    Left e  -> warn Normal e >> return (runtimeError e)
genEx tailpos (StgLet bind expr) = do
  genBindRec bind
  genEx tailpos expr
genEx tailpos (StgLetNoEscape _ _ bind expr) = do
  genBindRec bind
  genEx tailpos expr
genEx tailpos (StgCase ex _ _ bndr _ t alts) = do
  genCase tailpos ex bndr t alts
genEx tailpos (StgSCC _ _ _ ex) = do
  genEx tailpos ex
genEx tailpos (StgTick _ _ ex) = do
  genEx tailpos ex
genEx _ (StgLam _ _) =
  error "StgLam shouldn't happen during CG!"

genCase :: Bool -> StgExpr -> Id -> AltType -> [StgAlt] -> JSGen Config JSExp
genCase tailpos ex scrut t alts = do
    ex' <- genEx False ex
    -- If we have a unary unboxed tuple, we want to eliminate the case
    -- entirely (modulo evaluation), so just generate the expression in the
    -- sole alternative.
    case (isUnaryUnboxedTuple scrut, alts) of
      (True, [(_, as, _, expr)]) | [arg] <- filter hasRepresentation as -> do
        arg' <- genVar arg
        addLocal [arg']
        emit $ NewVar (AST.Var arg') ex'
        genEx tailpos expr
      (True, _) ->
        error "Case on unary unboxed tuple with more than one alt! WTF?!"
      _ -> do
        scrut' <- genVar scrut
        emit $ NewVar (AST.Var scrut') ex'
        res <- genResultVar scrut
        addLocal [scrut', res]
        genAlts tailpos t scrut' res alts

-- | Returns True if the given Var is an unboxed tuple with a single element
--   after any represenationless elements are discarded.
isUnaryUnboxedTuple :: Var -> Bool
isUnaryUnboxedTuple v = maybe False id $ do
    (_, args) <- splitTyConApp_maybe t
    case filter typeHasRep args of
      [_] -> return $ isUnboxedTupleType t
      _   -> return False
  where
    t = varType v

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
    DEFAULT                            -> return Def
    LitAlt l                           -> Cond <$> genLit l
    DataAlt c | tag <- genDataConTag c -> return $ Cond tag
  (args', used') <- genArgVarsPair (zip args used)
  addLocal args'
  let binds = [bindVar v ix | (v, True, ix) <- zip3 args' used' [1..]]
  (ret, body') <- isolate $ genEx tailpos body
  return $ construct
         $ bagToList
         $ listToBag binds `unionBags` body' `snocBag` NewVar (AST.Var res) ret
  where
    bindVar var ix = NewVar (AST.Var var) (Index (AST.Var scrut) (litN ix))

-- | Generate an argument list. Any arguments of type State# a are filtered out.
genArgs :: [StgArg] -> JSGen Config [JSExp]
genArgs = mapM genArg . filter hasRep
  where
    hasRep (StgVarArg v) = hasRepresentation v
    hasRep _             = True

-- | Filter out args without representation, along with their accompanying
--   pair element, then generate code for the args.
--   Se `genArgVarsPair` for more information.
genArgsPair :: [(StgArg, a)] -> JSGen Config ([JSExp], [a])
genArgsPair aps = do
    args' <- mapM genArg args
    return (args', xs)
  where
    (args, xs) = unzip $ filter hasRep aps
    hasRep (StgVarArg v, _) = hasRepresentation v
    hasRep _                = True

-- | Returns True if the given var actually has a representation.
--   Currently, only values of type State# a are considered representationless.
hasRepresentation :: Var -> Bool
hasRepresentation = typeHasRep . varType

typeHasRep :: Type -> Bool
typeHasRep t =
  case splitTyConApp_maybe t of
    Just (tc, _) -> tc /= statePrimTyCon
    _            -> True

genArg :: StgArg -> JSGen Config JSExp
genArg (StgVarArg v)  = AST.Var <$> genVar v
genArg (StgLitArg l)  = genLit l

-- | Generate a JS \xXX or \uXXXX escape sequence for a char if it's >127.
toHex :: Char -> String
toHex c =
  case ord c of
    n | n < 127   -> [c]
      | otherwise -> toHex' (n `rem` 65536)
  where
    toHex' n =
      case toH "" n of
        s@(_:_:[]) -> "\\x" ++ s
        s          -> "\\u" ++ s

    toH s 0 = s
    toH s n = case n `quotRem` 16 of
                (next, c) -> toH (i2h c : s) next

    i2h n | n < 10    = chr (n + 48)
          | otherwise = chr (n + 87)

-- | Escape all non-ASCII characters in the given string.
hexifyString :: FastString -> String
hexifyString = concatMap toHex . unpackFS

genLit :: Literal -> JSGen Config JSExp
genLit l = do
  case l of
    MachStr s           -> return . lit $ hexifyString s
    MachInt n
      | n > 2147483647 ||
        n < -2147483648 -> do warn Normal (constFail "Int" n)
                              return $ truncInt n
      | otherwise       -> return . litN $ fromIntegral n
    MachFloat f         -> return . litN $ fromRational f
    MachDouble d        -> return . litN $ fromRational d
    MachChar c          -> return . litN $ fromIntegral $ ord c
    MachWord w
      | w > 0xffffffff  -> do warn Normal (constFail "Word" w)
                              return $ truncWord w
      | otherwise       -> return . litN $ fromIntegral w
    MachWord64 w        -> return . litN $ fromIntegral w
    MachNullAddr        -> return $ litN 0
    MachInt64 n         -> return . litN $ fromIntegral n
    LitInteger n _      -> return . lit  $ n
    MachLabel _ _ _     -> return $ lit ":(" -- Labels point to machine code - ignore!
  where
    constFail t n = t ++ " literal " ++ show n ++ " doesn't fit in 32 bits;"
                    ++ " truncating!"
    truncInt n  = litN . fromIntegral $ (fromIntegral n :: Int32)
    truncWord w = litN . fromIntegral $ (fromIntegral w :: Word32)

-- | Extracts the name of a foreign var.
foreignName :: ForeignCall -> String
foreignName (CCall (CCallSpec (StaticTarget str _ _) _ _)) =
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
          error $ "Var is neither foreign, local or global: " ++
                  showOutputable v
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
    unique = showOutputable $ nameUnique name

-- | Generate a list of function argument variables.
--   Vars of type State# a are ignored.
genArgVars :: [Var] -> JSGen Config [JSVar]
genArgVars = mapM genVar . filter hasRepresentation

-- | Filter a list of (Var, anything) pairs, generate JSVars from the Vars
--   and then return both lists.
--   Lists of vars are often accompanied by lists of strictness or usage
--   annotations, which need to be filtered for types without representation
--   as well.
genArgVarsPair :: [(Var, a)] -> JSGen Config ([JSVar], [a])
genArgVarsPair vps = do
    vs' <- mapM genVar vs
    return (vs', xs) 
  where
    (vs, xs) = unzip $ filter (hasRepresentation . fst) vps

-- | Generate a new variable and add a dependency on it to the function
--   currently being generated.
genVar :: Var -> JSGen Config JSVar
genVar var | hasRepresentation var = do
  thisMod <- getModName
  case toBuiltin var of
    Just var' -> return var'
    _         -> do
      let var' = toJSVar thisMod var Nothing
      dependOn var'
      return $! var'
genVar _ = do
  return $ JSVar "" (Foreign "_")

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
