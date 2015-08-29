{-# LANGUAGE TupleSections, PatternGuards, CPP, OverloadedStrings #-}
module Haste.CodeGen (generate) where
-- Misc. stuff
import Control.Applicative
import Control.Monad
import Data.Int
import Data.Bits
import Data.Word
import Data.Char
import Data.List (partition, foldl')
import Data.Maybe (isJust)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Set as S
import qualified Data.Map as M

-- STG/GHC stuff
import Language.Haskell.GHC.Simple as GHC
import FastString (unpackFS)

-- AST stuff
import Data.JSTarget as J hiding ((.&.))
import Data.JSTarget.AST as J (Exp (..), Stm (..), LHS (..))

-- General Haste stuff
import Haste.Config
import Haste.Monad
import Haste.Errors
import Haste.PrimOps
import Haste.Builtins

-- | Generate an abstract JS module from a codegen config and an STG module.
generate :: Config -> StgModule -> J.Module
generate cfg stg =
  J.Module {
      modPackageId   = BS.fromString $ GHC.modPackageKey stg,
      J.modName      = BS.fromString $ GHC.modName stg,
      modDeps        = foldl' insDep M.empty theMod,
      modDefs        = foldl' insFun M.empty theMod
    }
  where
    opt = if optimize cfg then optimizeFun else const id
    theMod = genAST cfg (GHC.modName stg) (modCompiledModule stg)

    insFun m (_, Assign (NewVar _ v@(Internal n _ _)) body _) =
      M.insert n (opt v body) m
    insFun m _ =
      m

    -- TODO: perhaps do dependency-based linking for externals as well?
    insDep m (ds, Assign (NewVar _ (Internal v _ _)) _ _) =
      M.insert v (S.delete v ds) m
    insDep m _ =
      m

-- | Generate JS AST for bindings.
genAST :: Config -> String -> [StgBinding] -> [(S.Set J.Name, Stm)]
genAST cfg modname binds =
    binds'
  where
    binds' =
      map (depsAndCode . genJS cfg modname . uncurry (genBind True))
      $ concatMap unRec
      $ binds
    depsAndCode (_, ds, locs, stm) = (ds S.\\ locs, stm stop)

-- | Check for builtins that should generate inlined code. At this point only
--   w2i and i2w.
genInlinedBuiltin :: GHC.Var -> [StgArg] -> JSGen Config (Maybe Exp)
genInlinedBuiltin f [x] = do
    x' <- genArg x
    return $ case (modname, varname) of
      (Just "GHC.HasteWordInt", "w2i") ->
        Just $ binOp BitAnd x' (litN 0xffffffff)
      (Just "GHC.HasteWordInt", "i2w") ->
        Just $ binOp ShrL x' (litN 0)
      _ ->
        Nothing
  where
    modname = moduleNameString . moduleName <$> nameModule_maybe (GHC.varName f)
    varname = occNameString $ nameOccName $ GHC.varName f
genInlinedBuiltin _ _ =
  return Nothing


-- | Generate code for an STG expression.
genEx :: StgExpr -> JSGen Config Exp
genEx (StgApp f xs) = do
  mex <- genInlinedBuiltin f xs
  case mex of
    Just ex -> return ex
    _       -> genApp f xs
genEx (StgLit l) = do
  genLit l
genEx (StgConApp con args) = do
  -- On 64 bit machines, GHC constructs small integers from Ints rather than
  -- Int64, so we need to deal with it or be unable to reliably create Int64
  -- or Integer values.
  case (dataConNameModule con, args) of
    (("S#", "GHC.Integer.Type"), [StgLitArg (MachInt n)]) | tooLarge n -> do
        return $ mkInteger n
    (("True", "GHC.Types"), []) -> do
        return $ lit True
    (("False", "GHC.Types"), []) -> do
        return $ lit False
    _ -> do
        (tag, stricts) <- genDataCon con
        (args', stricts') <- genArgsPair $ zip args stricts
        -- Don't create unboxed tuples with a single element.
        case (isNewtypeLikeCon con || isUnboxedTupleCon con, args') of
          (True, [arg]) -> return $ evaluate arg (head stricts')
          _             -> mkCon tag args' stricts'
  where
    mkInteger n =
        array [litN 1, callForeign "I_fromBits" [array [lit lo, lit hi]]]
      where
        lo = n .&. 0xffffffff
        hi = n `shiftR` 32
    tooLarge n = n > 2147483647 || n < -2147483648
    -- Always inline enum-likes, bools are true/false, not 1/0.
    mkCon l _ _ | isEnumerationDataCon con = return l
    mkCon tag as ss = return $ array (tag : zipWith evaluate as ss)
    evaluate arg True = eval arg
    evaluate arg _    = arg
genEx (StgOpApp op args _) = do
  args' <- genArgs args
  cfg <- getCfg
  let theOp = case op of
        StgPrimOp op' ->
          maybeTrace cfg opstr args' <$> genOp cfg op' args'
          where opstr = BS.fromString $ showOutputable cfg op'
        StgPrimCallOp (PrimCall f _) ->
          Right $ maybeTrace cfg fs args' $ callForeign fs args'
          where fs = BS.fromString $ unpackFS f
        StgFCallOp (CCall (CCallSpec (StaticTarget f _ _) _ _)) _t ->
          Right $ maybeTrace cfg fs args' $ callForeign fs args'
          where fs = BS.fromString $ unpackFS f
        _ ->
          error $ "Tried to generate unsupported dynamic foreign call!"
  case theOp of
    Right x  -> return x
    Left err -> warn Normal err >> return (runtimeError err)
genEx (StgLet bind ex) = do
  genBindRec bind
  genEx ex
genEx (StgLetNoEscape _ _ bind ex) = do
  genBindRec bind
  genEx ex
genEx (StgCase ex _ _ bndr _ t alts) = do
  genCase t ex bndr alts
  
#if __GLASGOW_HASKELL__ < 710
-- StgSCC is gone in 7.10, and StgTick has an argument less.
genEx (StgSCC _ _ _ ex) = do
  genEx ex
genEx (StgTick _ _ ex) = do
#else
genEx (StgTick _ ex) = do
#endif
  genEx ex

genEx (StgLam _ _) = do
  error "StgLam caught during code generation - that's impossible!"
-- | Trace the given expression, if tracing is on.
maybeTrace :: Config -> BS.ByteString -> [Exp] -> Exp -> Exp
maybeTrace cfg msg args ex =
  if tracePrimops cfg
    then callForeign "__h_trace" [lit msg, array args, ex]
    else ex

genBindRec :: StgBinding -> JSGen Config ()
genBindRec bs@(StgRec _) = do
    mapM_ (genBind False (Just len) . snd) bs'
  where
    bs' = unRec bs
    len = length bs'
genBindRec b =
  genBind False Nothing b

-- | Generate code for all bindings. genBind spits out an error if it receives
--   a recursive binding; this is because it's quite a lot easier to keep track
--   of which functions depend on each other if every genBind call results in a
--   single function being generated.
--   Use `genBindRec` to generate code for local potentially recursive bindings
--   as their dependencies get merged into their parent's anyway.
genBind :: Bool -> Maybe Int -> StgBinding -> JSGen Config ()
genBind onTopLevel funsInRecGroup (StgNonRec v rhs) = do
  v' <- genVar v
  pushBind v'
  when (not onTopLevel) $ do
    addLocal v'
  expr <- genRhs (isJust funsInRecGroup) rhs
  popBind
  continue $ newVar True v' expr
genBind _ _ (StgRec _) =
  error $  "genBind got recursive bindings!"

-- | Generate the RHS of a binding.
genRhs :: Bool -> StgRhs -> JSGen Config Exp
genRhs recursive (StgRhsCon _ con args) = do
  -- Constructors are never partially applied, and we have arguments, so this
  -- is obviously a full application.
  if recursive
     then thunk True . ret <$> genEx (StgConApp con args)
     else genEx (StgConApp con args)
genRhs _ (StgRhsClosure _ _ _ upd _ args body) = do
    args' <- mapM genVar args
    (retExp, body') <- isolate $ do
      mapM_ addLocal args'
      genEx body
    return $ if null args
               then thunk' upd (body' $ thunkRet retExp)
               else fun args' (body' $ ret retExp)
  where
    thunk' _ (Return l@(J.Lit _)) = l
    thunk' Updatable stm          = thunk True stm
    thunk' ReEntrant stm          = thunk True stm
    thunk' SingleEntry stm        = thunk False stm

-- | Turn a recursive binding into a list of non-recursive ones, together with
--   information about whether they came from a recursive group or not.
unRec :: StgBinding -> [(Maybe Int, StgBinding)]
unRec (StgRec bs) = zip (repeat len) (map (uncurry StgNonRec) bs)
  where
    len = Just $ length bs
unRec b           = [(Nothing, b)]

-- | Filter a list of (Var, anything) pairs, generate JSVars from the Vars
--   and then return both lists.
--   Lists of vars are often accompanied by lists of strictness or usage
--   annotations, which need to be filtered for types without representation
--   as well.
genArgVarsPair :: [(GHC.Var, a)] -> JSGen Config ([J.Var], [a])
genArgVarsPair vps = do
    vs' <- mapM genVar vs
    return (vs', xs)
  where
    (vs, xs) = unzip $ filter (hasRepresentation . fst) vps

genCase :: AltType -> StgExpr -> Id -> [StgAlt] -> JSGen Config Exp
genCase t ex scrut alts = do
  cfg <- getCfg
  ex' <- genEx ex
  -- Return a scrutinee variable and a function to replace all occurrences of
  -- the STG scrutinee with our JS one, if needed.
  (scrut', withScrutinee) <- case ex' of
    Eval (J.Var v) | overwriteScrutinees cfg -> do
      continue $ assignVar (reorderableType scrut) v ex'
      oldscrut <- genVar scrut
      return (v, rename oldscrut v)
    _ -> do
      scrut' <- genVar scrut
      addLocal scrut'
      continue $ newVar (reorderableType scrut) scrut' ex'
      return (scrut', id)
  -- If we have a unary unboxed tuple, we want to eliminate the case
  -- entirely (modulo evaluation), so just generate the expression in the
  -- sole alternative.
  withScrutinee $ do
    case (isNewtypeLike scrut, isUnaryUnboxedTuple scrut, alts) of
      (_, True, [(_, as, _, expr)]) | [arg] <- filter hasRepresentation as -> do
        arg' <- genVar arg
        addLocal arg'
        continue $ newVar (reorderableType scrut) arg' (varExp scrut')
        genEx expr
      (True, _, [(_, [arg], _, expr)]) -> do
        arg' <- genVar arg
        addLocal arg'
        continue $ newVar (reorderableType scrut) arg' (varExp scrut')
        genEx expr
      (_, True, _) -> do
        error "Case on unary unboxed tuple with more than one alt! WTF?!"
      _ -> do
        -- Generate scrutinee and result vars
        res <- genResultVar scrut
        addLocal res
        -- Split alts into default and general, and generate code for them
        let (defAlt, otherAlts) = splitAlts alts
            scrutinee = cmp (varExp scrut')
        (_, defAlt') <- genAlt scrut' res defAlt
        alts' <- mapM (genAlt scrut' res) otherAlts
        -- Use the ternary operator where possible.
        useSloppyTCE <- sloppyTCE `fmap` getCfg
        self <- if useSloppyTCE then return blackHoleVar else getCurrentBinding
        case tryTernary self scrutinee (varExp res) defAlt' alts' of
          Just ifEx -> do
            continue $ newVar True res ifEx
            return (varExp res)
          _ -> do
            continue $ case_ scrutinee defAlt' alts'
            return (varExp res)
  where
    getTag s = index s (litN 0)
    cmp = case t of
      PrimAlt _ -> id
      AlgAlt tc -> if isEnumerationTyCon tc then id else getTag
      _         -> getTag


-- | Split a list of StgAlts into (default, [rest]). Since all case expressions
--   are total, if there is no explicit default branch, the last conditional
--   branch is the default one.
splitAlts :: [StgAlt] -> (StgAlt, [StgAlt])
splitAlts alts =
    case partition isDefault alts of
      ([defAlt], otherAlts) -> (defAlt, otherAlts)
      ([], otherAlts)       -> (last otherAlts, init otherAlts)
      _                     -> error "More than one default alt in case!"
  where
    isDefault (DEFAULT, _, _, _) = True
    isDefault _                  = False

genAlt :: J.Var -> J.Var -> StgAlt -> JSGen Config (Exp, Stm -> Stm)
genAlt scrut res (con, args, used, body) = do
  construct <- case con of
    -- undefined is intentional here - the first element is never touched.
    DEFAULT                            -> return (undefined, )
    LitAlt l                           -> (,) <$> genLit l
    DataAlt c | tag <- genDataConTag c -> return (tag, )
  (args', used') <- genArgVarsPair (zip args used)
  addLocal args'
  let binds = [bindVar v ix | (v, True, ix) <- zip3 args' used' [1..]]
  (_, body') <- isolate $ do
    continue $ foldr (.) id binds
    retEx <- genEx body
    continue $ newVar False res retEx
  return $ construct body'
  where
    bindVar v ix = newVar True v (index (varExp scrut) (litN ix))

-- | Generate a result variable for the given scrutinee variable.
genResultVar :: GHC.Var -> JSGen Config J.Var
genResultVar v = do
  v' <- genVar v >>= getActualName
  case v' of
    Foreign n ->
      return $ Internal (Name (BS.append n "#result") Nothing) "" True
    Internal (Name n mp) _ _ ->
      return $ Internal (Name (BS.append n "#result") mp) "" True

-- | Generate a new variable and add a dependency on it to the function
--   currently being generated.
genVar :: GHC.Var -> JSGen Config J.Var
genVar v | hasRepresentation v = do
  case toBuiltin v of
    Just v' -> return v'
    _       -> do
      mymod <- getModName
      v' <- getActualName $ toJSVar mymod v
      dependOn v'
      return v'
genVar _ = do
  return $ foreignVar "_"

-- | Extracts the name of a foreign var.
foreignName :: ForeignCall -> BS.ByteString
foreignName (CCall (CCallSpec (StaticTarget str _ _) _ _)) =
  BS.fromString $ unpackFS str
foreignName _ =
  error "Dynamic foreign calls not supported!"

-- | Turn a 'GHC.Var' into a 'J.Var'. Falls back to a default module name,
--   typically the name of the current module under compilation, if the given
--   Var isn't qualified.
toJSVar :: String -> GHC.Var -> J.Var
toJSVar thisMod v =
  case idDetails v of
    FCallId fc -> foreignVar (foreignName fc)
    _
      | isLocalId v && not hasMod ->
        internalVar (name unique (Just (myPkg, myMod))) ""
      | isGlobalId v || hasMod ->
        internalVar (name extern (Just (myPkg, myMod))) comment
    _ ->
      error $ "Var is not local, global or external!"
  where
    comment = BS.concat [myMod, ".", extern]
    vname  = GHC.varName v
    hasMod = case nameModule_maybe vname of
               Nothing -> False
               _       -> True
    myMod = BS.fromString $ maybe thisMod (moduleNameString . moduleName)
                                          (nameModule_maybe vname)
    myPkg = BS.fromString $ maybe "main" (pkgKeyString . modulePkgKey)
                                         (nameModule_maybe vname)
    extern = BS.fromString $ occNameString $ nameOccName vname
    unique = BS.fromString $ show $ nameUnique vname

-- | Generate an argument list. Any arguments of type State# a are filtered out.
genArgs :: [StgArg] -> JSGen Config [Exp]
genArgs = mapM genArg . filter hasRep
  where
    hasRep (StgVarArg v) = hasRepresentation v
    hasRep _             = True

-- | Filter out args without representation, along with their accompanying
--   pair element, then generate code for the args.
--   Se `genArgVarsPair` for more information.
genArgsPair :: [(StgArg, a)] -> JSGen Config ([Exp], [a])
genArgsPair aps = do
    args' <- mapM genArg args
    return (args', xs)
  where
    (args, xs) = unzip $ filter hasRep aps
    hasRep (StgVarArg v, _) = hasRepresentation v
    hasRep _                = True

-- | Returns True if the given var actually has a representation.
--   Currently, only values of type State# a are considered representationless.
hasRepresentation :: GHC.Var -> Bool
hasRepresentation = typeHasRep . varType

typeHasRep :: Type -> Bool
typeHasRep t =
  case splitTyConApp_maybe t of
    Just (tc, _) -> tc /= statePrimTyCon
    _            -> True

genArg :: StgArg -> JSGen Config Exp
genArg (StgVarArg v)  = varExp <$> genVar v
genArg (StgLitArg l)  = genLit l

-- | Generate code for data constructor creation. Returns a pair of
--   (constructor, field strictness annotations).
genDataCon :: DataCon -> JSGen Config (Exp, [Bool])
genDataCon dc = do
  if isEnumerationDataCon dc
    then return (tagexp, [])
    else return (tagexp, map strict (dataConRepStrictness dc))
  where
    tagexp = genDataConTag dc
    strict MarkedStrict = True
    strict _            = False

-- | Generate the tag for a data constructor. This is used both by genDataCon
--   and directly by genCase to generate constructors for matching.
--
--   IMPORTANT: remember to update the RTS if any changes are made to the
--              constructor tag values!
genDataConTag :: DataCon -> Exp
genDataConTag d =
  case dataConNameModule d of
    ("True", "GHC.Types")  -> lit True
    ("False", "GHC.Types") -> lit False
    _                      ->
      lit (fromIntegral (dataConTag d - fIRST_TAG) :: Double)

-- | Get the name and module of the given data constructor.
dataConNameModule :: DataCon -> (String, String)
dataConNameModule d =
  (occNameString $ nameOccName $ dataConName d,
   moduleNameString $ moduleName $ nameModule $ dataConName d)


-- | Generate literals.
genLit :: GHC.Literal -> JSGen Config Exp
genLit l = do
  case l of
    MachStr s           -> return $ lit s
    MachInt n
      | n > 2147483647 ||
        n < -2147483648 -> do warn Verbose (constFail "Int" n)
                              return $ truncInt n
      | otherwise       -> return . litN $ fromIntegral n
    MachFloat f         -> return . litN $ fromRational f
    MachDouble d        -> return . litN $ fromRational d
    MachChar c          -> return . litN $ fromIntegral $ ord c
    MachWord w
      | w > 0xffffffff  -> do warn Verbose (constFail "Word" w)
                              return $ truncWord w
      | otherwise       -> return . litN $ fromIntegral w
    MachWord64 w        -> return $ word64 w
    MachNullAddr        -> return $ litN 0
    MachInt64 n         -> return $ int64 n
    LitInteger n _      -> return $ lit n
    -- Labels point to machine code - ignore!
    MachLabel _ _ _     -> return $ litS ":("
  where
    constFail t n = t ++ " literal " ++ show n ++ " doesn't fit in 32 bits;"
                    ++ " truncating!"
    truncInt n  = litN . fromIntegral $ (fromIntegral n :: Int32)
    truncWord w = litN . fromIntegral $ (fromIntegral w :: Word32)
    int64 n = callForeign "new Long" [lit lo, lit hi]
      where
        lo = n .&. 0xffffffff
        hi = n `shiftR` 32
    word64 n = callForeign "I_fromBits" [array [lit lo, lit hi]]
      where
        lo = n .&. 0xffffffff
        hi = n `shiftR` 32

-- | Generate a function application.
genApp :: GHC.Var -> [StgArg] -> JSGen Config Exp
genApp f xs = do
    f' <- varExp <$> genVar f
    xs' <- mapM genArg xs
    if null xs
      then return $ eval f'
      else return $ call arity f' xs'
  where
    arity = arityInfo $ idInfo f

-- | Does this data constructor create an enumeration type?
isEnumerationDataCon :: DataCon -> Bool
isEnumerationDataCon = isEnumerationTyCon . dataConTyCon

-- | Does this data constructor create a newtype-like value? That is, a value
--   of a type with a single data constructor having a single argument?
isNewtypeLikeCon :: DataCon -> Bool
isNewtypeLikeCon c =
  case tyConDataCons (dataConTyCon c) of
    [_] -> case dataConRepArgTys c of
      [t] -> isUnLiftedType t
      _   -> False
    _   -> False

-- | Does this data constructor create a newtype-like value? That is, a value
--   of a type with a single data constructor having a single unlifted
--   argument?
isNewtypeLike :: GHC.Var -> Bool
isNewtypeLike v = maybe False id $ do
  (tycon, _) <- splitTyConApp_maybe (varType v)
  case tyConDataCons tycon of
    [c] -> case dataConRepArgTys c of
      [t] -> return (isUnLiftedType t)
      _   -> return False
    _   -> return False

-- | Returns True if the given Var is an unboxed tuple with a single element
--   after any represenationless elements are discarded.
isUnaryUnboxedTuple :: GHC.Var -> Bool
isUnaryUnboxedTuple v = maybe False id $ do
    (_, args) <- splitTyConApp_maybe t
    case filter typeHasRep args of
      [_] -> return $ isUnboxedTupleType t
      _   -> return False
  where
    t = varType v

-- | Is it safe to reorder values of the given type?
reorderableType :: GHC.Var -> Bool
reorderableType v =
    case splitTyConApp_maybe t of
      Just (_, args) -> length (filter typeHasRep args) == length args
      _              -> typeHasRep t
  where
    t = varType v
