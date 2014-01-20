{-# LANGUAGE TupleSections, PatternGuards, CPP #-}
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
import qualified Data.Set as S
import qualified Data.Map as M
-- STG/GHC stuff
import StgSyn
import CoreSyn (AltCon (..))
import Var (Var, varType, varName)
import IdInfo (arityInfo, IdDetails (..))
import Id (Id, idInfo, idDetails, isLocalId, isGlobalId)
import Literal as L
import FastString (unpackFS)
import ForeignCall (CCallTarget (..), ForeignCall (..), CCallSpec (..))
import PrimOp (PrimCall (..))
import OccName
import DataCon
import Module
import Name
import Type
import TysPrim
import TyCon
import BasicTypes
-- AST stuff
import Data.JSTarget as J hiding ((.&.))
import Data.JSTarget.AST (Exp (..), Stm (..), LHS (..))
-- General Haste stuff
import Haste.Config
import Haste.Monad
import Haste.Errors
import Haste.PrimOps
import Haste.Builtins
import Haste.Util (showOutputable)

generate :: Config
         -> Fingerprint
         -> String
         -> ModuleName
         -> [StgBinding]
         -> J.Module
generate cfg fp pkgid modname binds =
  Module {
      modFingerprint = fp,
      modPackageId   = pkgid,
      modName        = moduleNameString modname,
      modDeps        = foldl' insDep M.empty theMod,
      modDefs        = foldl' insFun M.empty theMod
    }
  where
    theMod = genAST cfg modname binds
    
    insFun m (_, AST (Assign (NewVar _ (Internal v _)) body _) jumps) =
      M.insert v (AST body jumps) m
    insFun m _ =
      m

    -- TODO: perhaps do dependency-based linking for externals as well?
    insDep m (ds, AST (Assign (NewVar _ (Internal v _)) _ _) _) =
      M.insert v (S.delete v ds) m
    insDep m _ =
      m

-- | Generate JS AST for bindings.
genAST :: Config -> ModuleName -> [StgBinding] -> [(S.Set J.Name, AST Stm)]
genAST cfg modname binds =
    binds'
  where
    binds' =
      map (depsAndCode . genJS cfg myModName . uncurry (genBind True))
      $ concatMap unRec
      $ binds
    myModName = moduleNameString modname
    depsAndCode (_, ds, locs, stm) = (ds S.\\ locs, stm nullRet)

-- | Check for builtins that should generate inlined code. At this point only
--   w2i and i2w.
genInlinedBuiltin :: Var.Var -> [StgArg] -> JSGen Config (Maybe (AST Exp))
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
    modname = moduleNameString . moduleName <$> nameModule_maybe (Var.varName f)
    varname = occNameString $ nameOccName $ Var.varName f
genInlinedBuiltin _ _ =
  return Nothing


-- | Generate code for an STG expression.
genEx :: StgExpr -> JSGen Config (AST Exp)
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
    _ -> do
      (tag, stricts) <- genDataCon con
      (args', stricts') <- genArgsPair $ zip args stricts
      -- Don't create unboxed tuples with a single element.
      case (isUnboxedTupleCon con, args') of
        (True, [arg]) -> return $ evaluate arg (head stricts')
        _             -> mkCon tag args' stricts'
  where
    mkInteger n =
        array [litN 1, callForeign "I_fromBits" [array [lit lo, lit hi]]]
      where
        lo = n .&. 0xffffffff
        hi = n `shiftR` 32
    tooLarge n = n > 2147483647 || n < -2147483648
    -- Always inline bools
    mkCon l _ _ | isEnumerationDataCon con = return l
    mkCon tag as ss = return $ array (tag : zipWith evaluate as ss)
    evaluate arg True = eval arg
    evaluate arg _    = arg
genEx (StgOpApp op args _) = do
  args' <- genArgs args
  cfg <- getCfg
  let theOp = case op of
        StgPrimOp op' ->
          maybeTrace cfg (showOutputable op') args' <$> genOp cfg op' args'
        StgPrimCallOp (PrimCall f _) ->
          Right $ maybeTrace cfg fs args' $ callForeign fs args'
          where fs = unpackFS f
#if __GLASGOW_HASKELL__ >= 706
        StgFCallOp (CCall (CCallSpec (StaticTarget f _ _) _ _)) _t ->
#else
        StgFCallOp (CCall (CCallSpec (StaticTarget f _) _ _)) _t ->
#endif
          Right $ maybeTrace cfg fs args' $ callForeign fs args'
          where fs = unpackFS f
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
genEx (StgSCC _ _ _ ex) = do
  genEx ex
genEx (StgTick _ _ ex) = do
  genEx ex
#if __GLASGOW_HASKELL__ >= 706
genEx (StgLam _ _) = do
  error "StgLam caught during code generation - that's impossible!"
#else
genEx (StgLam _ _ _) = do
  error "StgLam caught during code generation - that's impossible!"
#endif
-- | Trace the given expression, if tracing is on.
maybeTrace :: Config -> String -> [AST Exp] -> AST Exp -> AST Exp
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
  let expr' = optimizeFun v' expr
  continue $ newVar True v' expr'
genBind _ _ (StgRec _) =
  error $  "genBind got recursive bindings!"

-- | Generate the RHS of a binding.
genRhs :: Bool -> StgRhs -> JSGen Config (AST Exp)
genRhs recursive (StgRhsCon _ con args) = do
  -- Constructors are never partially applied, and we have arguments, so this
  -- is obviously a full application.
  if recursive
     then thunk . ret <$> genEx (StgConApp con args)
     else genEx (StgConApp con args)
genRhs _ (StgRhsClosure _ _ _ upd _ args body) = do
    args' <- mapM genVar args
    (retExp, body') <- isolate $ do
      mapM_ addLocal args'
      genEx body
    return $ if isUpdatable upd && null args
               then thunk' (body' $ ret retExp)
               else fun args' (body' $ ret retExp)
  where
    thunk' (AST (Return l@(Lit _)) js) = AST l js
    thunk' stm                         = thunk stm

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
genArgVarsPair :: [(Var.Var, a)] -> JSGen Config ([J.Var], [a])
genArgVarsPair vps = do
    vs' <- mapM genVar vs
    return (vs', xs) 
  where
    (vs, xs) = unzip $ filter (hasRepresentation . fst) vps

genCase :: AltType -> StgExpr -> Id -> [StgAlt] -> JSGen Config (AST Exp)
genCase t ex scrut alts = do
  ex' <- genEx ex
  -- If we have a unary unboxed tuple, we want to eliminate the case
  -- entirely (modulo evaluation), so just generate the expression in the
  -- sole alternative.
  case (isUnaryUnboxedTuple scrut, alts) of
    (True, [(_, as, _, expr)]) | [arg] <- filter hasRepresentation as -> do
      arg' <- genVar arg
      addLocal [arg']
      continue (newVar (reorderableType scrut) arg' ex')
      genEx expr
    (True, _) -> do
        error "Case on unary unboxed tuple with more than one alt! WTF?!"
    _ -> do
      -- Generate scrutinee and result vars
      scrut' <- genVar scrut
      res <- genResultVar scrut
      addLocal [scrut', res]
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
          continue $ newVar (reorderableType scrut) scrut' ex'
          continue $ newVar True res ifEx
          return (varExp res)
        _ -> do
          continue $ newVar (reorderableType scrut) scrut' ex'
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

genAlt :: J.Var -> J.Var -> StgAlt -> JSGen Config (AST Exp,AST Stm -> AST Stm)
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
    continue $ newVar True res retEx
  return $ construct body'
  where
    bindVar v ix = newVar True v (index (varExp scrut) (litN ix))

-- | Generate a result variable for the given scrutinee variable.
genResultVar :: Var.Var -> JSGen Config J.Var
genResultVar v = (\mn -> toJSVar mn v (Just "#result")) <$> getModName

-- | Generate a new variable and add a dependency on it to the function
--   currently being generated.
genVar :: Var.Var -> JSGen Config J.Var
genVar v | hasRepresentation v = do
  case toBuiltin v of
    Just v' -> return v'
    _       -> do
      mymod <- getModName
      v' <- return $ toJSVar mymod v Nothing
      dependOn v'
      return v'
genVar _ = do
  return $ foreignVar "_"

-- | Extracts the name of a foreign var.
foreignName :: ForeignCall -> String
#if __GLASGOW_HASKELL__ >= 706
foreignName (CCall (CCallSpec (StaticTarget str _ _) _ _)) =
  unpackFS str
#else
foreignName (CCall (CCallSpec (StaticTarget str _) _ _)) =
  unpackFS str
#endif
foreignName _ =
  error "Dynamic foreign calls not supported!"

toJSVar :: String -> Var.Var -> Maybe String -> J.Var
toJSVar thisMod v msuffix =
  case idDetails v of
    FCallId fc -> foreignVar (foreignName fc)
    _
      | isLocalId v && not hasMod ->
        internalVar (name (unique ++ suffix) (Just (myPkg, myMod))) ""
      | isGlobalId v || hasMod ->
        internalVar (name (extern ++ suffix) (Just (myPkg, myMod))) comment
    _ ->
      error $ "Var is not local, global or external!"
  where
    comment = myMod ++ "." ++ extern ++ suffix
    suffix = case msuffix of
               Just s -> s
               _      -> ""
    vname  = Var.varName v
    hasMod = case nameModule_maybe vname of
               Nothing -> False
               _       -> True
    myMod =
      maybe thisMod (moduleNameString . moduleName) (nameModule_maybe vname)
    myPkg =
      maybe "main" (showOutputable . modulePackageId) (nameModule_maybe vname)
    extern = occNameString $ nameOccName vname
    unique = show $ nameUnique vname

-- | Generate an argument list. Any arguments of type State# a are filtered out.
genArgs :: [StgArg] -> JSGen Config [AST Exp]
genArgs = mapM genArg . filter hasRep
  where
    hasRep (StgVarArg v) = hasRepresentation v
    hasRep _             = True

-- | Filter out args without representation, along with their accompanying
--   pair element, then generate code for the args.
--   Se `genArgVarsPair` for more information.
genArgsPair :: [(StgArg, a)] -> JSGen Config ([AST Exp], [a])
genArgsPair aps = do
    args' <- mapM genArg args
    return (args', xs)
  where
    (args, xs) = unzip $ filter hasRep aps
    hasRep (StgVarArg v, _) = hasRepresentation v
    hasRep _                = True

-- | Returns True if the given var actually has a representation.
--   Currently, only values of type State# a are considered representationless.
hasRepresentation :: Var.Var -> Bool
hasRepresentation = typeHasRep . varType

typeHasRep :: Type -> Bool
typeHasRep t =
  case splitTyConApp_maybe t of
    Just (tc, _) -> tc /= statePrimTyCon
    _            -> True

genArg :: StgArg -> JSGen Config (AST Exp)
genArg (StgVarArg v)  = varExp <$> genVar v
genArg (StgLitArg l)  = genLit l
#if __GLASGOW_HASKELL__ < 706
genArg (StgTypeArg t) = do
  warn Normal "Generated StgTypeArg as 0!"
  return (litN 0)
#endif

-- | Generate code for data constructor creation. Returns a pair of
--   (constructor, field strictness annotations).
genDataCon :: DataCon -> JSGen Config (AST Exp, [Bool])
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
genDataConTag :: DataCon -> AST Exp
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
genLit :: L.Literal -> JSGen Config (AST Exp)
genLit l = do
  case l of
    MachStr s           -> return . lit $ unpackFS s
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
    MachLabel _ _ _     -> return $ lit ":(" -- Labels point to machine code - ignore!
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
genApp :: Var.Var -> [StgArg] -> JSGen Config (AST Exp)
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

-- | Returns True if the given Var is an unboxed tuple with a single element
--   after any represenationless elements are discarded.
isUnaryUnboxedTuple :: Var.Var -> Bool
isUnaryUnboxedTuple v = maybe False id $ do
    (_, args) <- splitTyConApp_maybe t
    case filter typeHasRep args of
      [_] -> return $ isUnboxedTupleType t
      _   -> return False
  where
    t = varType v

-- | Is it safe to reorder values of the given type?
reorderableType :: Var.Var -> Bool
reorderableType v =
    case splitTyConApp_maybe t of
      Just (_, args) -> length (filter typeHasRep args) == length args
      _              -> typeHasRep t
  where
    t = varType v
