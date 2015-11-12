-- | Basic data flow analysis over the Haste AST.
module Haste.AST.FlowAnalysis (
    Strict (..), VarInfo (..), InfoMap, ArgMap,
    mkVarInfo, nullInfo, mergeVarInfos, findVarInfos
  ) where
import Haste.AST.Syntax
import Control.Monad.State
import Data.List (foldl', sort, group)
import qualified Data.Set as S
import qualified Data.Map as M

data Strict = Strict | Unknown
  deriving (Eq, Show)

data VarInfo = VarInfo {
    -- | Is this var always strict?
    varStrict :: Strict,

    -- | Does this var always have the same, statically known, arity?
    --   @Nothing@, if var is not a function.
    varArity  :: Maybe Int,

    -- | What are the sources of this var? I.e. for each occurrence where the
    --   var is the LHS of an assignment or substitution (including function
    --   call), add the RHS to this list.
    varAlias  :: [Var]
  } deriving (Eq, Show)

mkVarInfo :: Strict -> Maybe Int -> VarInfo
mkVarInfo s ar = VarInfo s ar []

type InfoMap = M.Map Var VarInfo
type ArgMap = M.Map Var [Var]

nullInfo :: VarInfo
nullInfo = VarInfo Unknown Nothing []

-- | Merge two var infos. The aliases of the infos are simply concatenated
--   and filtered for duplicates. For concrete information, if the two infos
--   disagree on any field, that field becomes unknown.
mergeVarInfos :: VarInfo -> VarInfo -> VarInfo
mergeVarInfos a b = VarInfo strict' arity' (varAlias a `merge` varAlias b)
  where
    strict'
      | varStrict a == varStrict b = varStrict a
      | otherwise                  = Unknown
    arity'
      | varArity a == varArity b   = varArity a
      | otherwise                  = Nothing
    merge as bs = map head . group . sort $ as ++ bs

-- | Merge the infos of a var with that of all of its aliases.
resolveVarInfo :: InfoMap -> Var -> Maybe VarInfo
resolveVarInfo m var = go (S.singleton var) var
  where
    go seen v = do
      nfo <- M.lookup v m
      let xs'   = filter (not . (`S.member` seen)) (varAlias nfo)
          seen' = foldl' (flip S.insert) seen xs'
      nfos <- mapM (go seen') xs'
      case foldl' mergeVarInfos (nfo {varAlias = xs'}) nfos of
        VarInfo Unknown Nothing _ -> Nothing
        nfo'                      -> return nfo'

type EvalM = State InfoMap

-- | Figure out as much statically known information as possible about all
--   vars in a program. The returned map will be collapsed so that recursive
--   lookups are not necessary, and the 'varAlias' field of each info will
--   be empty.
--
--   TODO: track return values, tail call status
findVarInfos :: ArgMap -> Stm -> InfoMap
findVarInfos argmap = mergeAll . snd . flip runState M.empty . goS
  where
    mergeAll m = M.foldWithKey resolve m m

    resolve v _ m =
      case resolveVarInfo m v of
        Just nfo -> M.insert v (nfo {varAlias = []}) m
        _        -> m

    goS Stop = return ()
    goS Cont = return ()
    goS (Forever s) = goS s
    goS (Case c d as next) = do
      goE c
      goS d
      mapM_ (\(e, s) -> goE e >> goS s) as
      goS next
    goS (Assign (NewVar _ v) rhs next)       = handleAssign v rhs >> goS next
    goS (Assign (LhsExp _ (Var v)) rhs next) = handleAssign v rhs >> goS next
    goS (Assign (LhsExp _ lhs) rhs next)     = goE lhs >> goE rhs >> goS next
    goS (Return ex)                          = goE ex
    goS (ThunkRet ex)                        = goE ex
    goS (Tailcall ex)                        = goE ex

    handleAssign v (Var v')        = v `dependsOn` v'
    handleAssign v (Eval (Var v')) = v `dependsOn` v'
    handleAssign v (Fun args body) = do
      v `hasInfo` mkVarInfo Strict (Just $ length args)
      goS body
    handleAssign v (Lit _)         = v `hasInfo` mkVarInfo Strict Nothing
    handleAssign v (JSLit _)       = v `hasInfo` mkVarInfo Strict Nothing
    handleAssign v rhs             = v `hasInfo` nullInfo >> goE rhs

    -- If a function is ever stored anywhere except as a pure alias, we must
    -- immediately stop assuming things about it, since we now have no idea
    -- where it may be called. If it is not stored, we know that all calls to
    -- it will be direct, so it is safe to assume things about its arguments
    -- based on our static analysis.
    -- To be safe - but overly conservative - we set the
    -- info of any arguments of each function aliased by a var that occurs
    -- outside a permitted context (function call or synonymous assignment)
    -- to nullInfo.
    goE (Var v)                  = setArgsOf v nullInfo (S.singleton v)
    goE (Lit _)                  = return ()
    goE (JSLit _)                = return ()
    goE (Not ex)                 = goE ex
    goE (BinOp _ a b)            = goE a >> goE b
    goE (Fun _ body)             = goS body
    goE (Call _ _ (Var f) xs)    = handleCall f xs
    goE (Call _ _ (Fun as b) xs) = handleArgs as xs >> goS b
    goE (Call _ _ f xs)          = mapM_ goE (f:xs)
    goE (Index x ix)             = goE x >> goE ix
    goE (Arr xs)                 = mapM_ goE xs
    goE (Member x _)             = goE x
    goE (Obj xs)                 = mapM_ goE (map snd xs)
    goE (AssignEx (Var v) rhs)   = handleAssign v rhs
    goE (AssignEx lhs rhs)       = goE lhs >> goE rhs
    goE (IfEx c l r)             = mapM_ goE [c, l, r]
    goE (Eval ex)                = goE ex
    goE (Thunk _ body)           = goS body

    handleArgs as xs = sequence_ (zipWith handleAssign as xs)

    handleCall f argexs =
      case M.lookup f argmap of
        Just argvars -> handleArgs argvars argexs
        _            -> return ()

    -- Finds the functions (if any) aliased by v and sets the varinfo of their
    -- arguments to @nfo@.
    setArgsOf v nfo seen = do
      case M.lookup v argmap of
        Just args -> mapM_ (`hasInfo` nullInfo) args
        _         -> do
          nfos <- get
          case M.lookup v nfos of
            Nothing   -> return ()
            Just nfo' -> do
              let aliases = filter (not . (`S.member` seen)) (varAlias nfo')
                  seen' = foldl' (flip S.insert) seen aliases
              mapM_ (\v' -> setArgsOf v' nfo seen') aliases

hasInfo :: Var -> VarInfo -> EvalM ()
hasInfo v nfo = do
    nfos <- get
    put $ M.alter upd v nfos
  where
    upd (Just nfo') = Just $ mergeVarInfos nfo nfo'
    upd _           = Just nfo

dependsOn :: Var -> Var -> EvalM ()
dependsOn v v' = do
    nfos <- get
    put $ M.alter upd v nfos
  where
    upd (Just nfo) = Just $ nfo {varAlias = v' : varAlias nfo}
    upd _          = Just $ nullInfo {varAlias = [v']}
