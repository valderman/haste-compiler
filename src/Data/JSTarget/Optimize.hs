{-# LANGUAGE PatternGuards, TupleSections, DoAndIfThenElse #-}
-- | Optimizations over the JSTarget AST.
module Data.JSTarget.Optimize (
    optimizeFun, tryTernary, topLevelInline
  ) where
import Data.JSTarget.AST
import Data.JSTarget.Op
import Data.JSTarget.Traversal
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S

-- TODO: tryTernary may inline calls that would otherwise be in tail position
--       which is something we'd really like to avoid.

optimizeFun :: Var -> AST Exp -> AST Exp
optimizeFun f (AST ast js) =
  flip runTravM js $ do
    shrinkCase ast
    >>= inlineAssigns
    >>= optimizeArrays
    >>= inlineReturns
    >>= zapJSStringConversions
    >>= optimizeThunks
    >>= optimizeArrays
    >>= tailLoopify f
    >>= ifReturnToTernary

topLevelInline :: AST Stm -> AST Stm
topLevelInline (AST ast js) =
  flip runTravM js $ do
    inlineAssigns ast
    >>= optimizeArrays
    >>= optimizeThunks
    >>= optimizeArrays

-- | Attempt to turn two case branches into a ternary operator expression.
tryTernary :: Var
           -> AST Exp
           -> AST Exp
           -> (AST Stm -> AST Stm)
           -> [(AST Exp, AST Stm -> AST Stm)]
           -> Maybe (AST Exp)
tryTernary self scrut retEx def [(m, alt)] =
    case runTravM opt allJumps of
      AST (Just ex) js -> Just (AST ex js)
      _                -> Nothing
  where
    selfOccurs (Exp (Var v)) = v == self
    selfOccurs _             = False
    def' = def $ Return <$> retEx
    alt' = alt $ Return <$> retEx
    AST _ allJumps = scrut >> m >> def' >> alt'
    opt = do
      -- Make sure the return expression is used somewhere, then cut away all
      -- useless assignments. If what's left is a single Return statement,
      -- we have a pure expression suitable for use with ?:.
      def'' <- inlineAssignsLocal $ astCode def'
      alt'' <- inlineAssignsLocal $ astCode alt'
      -- If self occurs in either branch, we can't inline or we risk ruining
      -- tail call elimination.
      selfInDef <- occurrences (const True) selfOccurs def''
      selfInAlt <- occurrences (const True) selfOccurs alt''
      case (selfInDef + selfInAlt, def'', alt'') of
        (Never, Return el, Return th) ->
          return $ Just $ IfEx (BinOp Eq (astCode scrut) (astCode m)) th el
        _ ->
          return Nothing
tryTernary _ _ _ _ _ =
  Nothing

-- | How many times does an expression satisfying the given predicate occur in
--   an AST (including jumps)?
occurrences :: JSTrav ast
            => (ASTNode -> Bool)
            -> (ASTNode -> Bool)
            -> ast
            -> TravM Occs
occurrences tr p ast =
    foldJS trav count Never ast
  where
    trav n node = tr node && n < Lots -- Stop traversal if we're already >1.
    count n node | p node = pure $ n + Once
    count n _             = pure n

-- | Replace all occurrences of an expression, without entering shared code
--   paths. IO ordering is preserved even when entering lambdas thanks to
--   State# RealWorld.
replaceEx :: JSTrav ast => (ASTNode -> Bool) -> Exp -> Exp -> ast -> TravM ast
replaceEx trav old new =
  mapJS trav (\x -> if x == old then pure new else pure x) pure

-- | Inline assignments where the assignee is only ever used once.
--   Does not inline anything into a shared code path, as that would break
--   things horribly.
--   Ignores LhsExp assignments, since we only introduce those when we actually
--   care about the assignment side effect.
--
--   TODO: don't inline thunks into functions!
inlineAssigns :: JSTrav ast => ast -> TravM ast
inlineAssigns ast = do
    inlinable <- gatherInlinable ast
    mapJS (const True) return (inl inlinable) ast
  where
    varOccurs lhs (Exp (Var lhs')) = lhs == lhs'
    varOccurs _ _                  = False
    inl m keep@(Assign (NewVar mayReorder lhs) ex next) = do
      occursRec <- occurrences (const True) (varOccurs lhs) ex
      if occursRec == Never
        then do
          occursLocal <- occurrences (not <$> isShared) (varOccurs lhs) next
          case M.lookup lhs m of
            Just occ | occ == occursLocal ->
              case occ of
                -- Inline of any non-lambda value
                Once | mayReorder -> do
                  replaceEx (not <$> isShared) (Var lhs) ex next
                -- Don't inline lambdas, but use less verbose syntax.
                _    | Fun Nothing vs body <- ex,
                       Internal lhsname _ <- lhs -> do
                  return $ Assign blackHole (Fun (Just lhsname) vs body) next
                _ -> do
                  return keep
            _ ->
              return keep
        else do
          return keep
    inl _ stm = return stm


-- | Turn if(foo) {return bar;} else {return baz;} into return foo ? bar : baz.
ifReturnToTernary :: JSTrav ast => ast -> TravM ast
ifReturnToTernary ast = do
    mapJS (const True) return opt ast
  where
    opt (Case cond (Return el) [(ex, Return th)] _) =
      pure $ Return $ IfEx (BinOp Eq cond ex) th el
    opt stm =
      pure stm

-- | Turn occurrences of [a,b][1] into b.
optimizeArrays :: JSTrav ast => ast -> TravM ast
optimizeArrays ast =
    mapJS (const True) inlEx return ast
  where
    inlEx (Index (Arr xs) (Lit (LNum n))) =
      return $ xs !! truncate n
    inlEx x =
      return x


-- | Turn toJSStr(unCStr(x)) into x, since rewrite rules absolutely refuse
--   to work with unpackCString#.
--   Also turn T(unCStr(x)) into unCStr(x) whenever x is a literal, since
--   unCStr is evaluated lazily anyway.
zapJSStringConversions :: JSTrav ast => ast -> TravM ast
zapJSStringConversions ast =
    mapJS (const True) opt return ast
  where
    opt (Call _ _ (Var (Foreign "toJSStr"))
       [Call _ _ (Var (Foreign "unCStr")) [x]]) =
      return x
    opt (Call _ _ (Var (Foreign "new T"))
         [Fun _ [] (Return x@(Call _ _ (Var (Foreign "unCStr")) [Lit _]))]) =
      return x
    opt x =
      return x


-- | Optimize thunks in the following ways:
--   A(thunk(return f), xs)
--     => A(f, xs)
--   E(thunk(return x))
--     => x
--   E(\x ... -> ...)
--     => \x ... -> ...
optimizeThunks :: JSTrav ast => ast -> TravM ast
optimizeThunks ast =
    mapJS (const True) optEx return ast
  where
    optEx (Call _ _ (Var (Foreign "E")) [x])
      | Just x' <- fromThunkEx x = return x'
      | Fun _ _ _ <- x           = return x
    optEx (Call arity calltype f args) | Just f' <- fromThunkEx f =
      return $ Call arity calltype f' args
    optEx ex =
      return ex


-- | Unpack the given expression if it's a thunk.
fromThunk :: Exp -> Maybe Stm
fromThunk (Call _ Fast (Var (Foreign "new T")) [body]) =
  case body of
    Fun Nothing [] (b) -> Just b
    _                  -> Nothing
fromThunk _ =
  Nothing

-- | Unpack the given expression if it's a thunk without internal bindings.
fromThunkEx :: Exp -> Maybe Exp
fromThunkEx ex =
  case fromThunk ex of
    Just (Return ex') -> Just ex'
    _                 -> Nothing


-- | Gather a map of all inlinable symbols; that is, the once that are used
--   exactly once.
--   TODO: always inline assigns that are just aliases!
gatherInlinable :: JSTrav ast => ast -> TravM (M.Map Var Occs)
gatherInlinable ast = do
    m <- foldJS (\_ _->True) countOccs (M.empty) ast
    return (M.filter (< Lots) m)
  where
    updVar (Just occs) = Just (occs+Once)
    updVar _           = Just Once
    updVarAss (Just o) = Just o
    updVarAss _        = Just Never
    countOccs m (Exp (Var v@(Internal _ _))) =
      pure (M.alter updVar v m)
    countOccs m (Stm (Assign (NewVar _ v) _ _)) =
      pure (M.alter updVarAss v m)
    countOccs m _ =
      pure m

-- | Like `inlineAssigns`, but doesn't care what happens beyond a jump.
inlineAssignsLocal :: JSTrav ast => ast -> TravM ast
inlineAssignsLocal ast = do
    mapJS (\n -> not (isLambda n || isShared n)) return inl ast
  where
    varOccurs lhs (Exp (Var lhs')) = lhs == lhs'
    varOccurs _ _                  = False
    inl keep@(Assign (NewVar mayReorder lhs) ex next) = do
      occurs <- occurrences (const True) (varOccurs lhs) next
      occurs' <- occurrences (const True) (varOccurs lhs) ex
      case occurs + occurs' of
        Never ->
          return (Assign blackHole ex next)
        -- Don't inline lambdas at the moment, but use less verbose syntax.
        _     | Fun Nothing vs body <- ex,
                Internal lhsname _ <- lhs ->
          return $ Assign blackHole (Fun (Just lhsname) vs body) next
        Once | mayReorder ->
          -- can't be recursive - inline
          replaceEx (not <$> isShared) (Var lhs) ex next
        _ ->
          -- Really nothing to be done here.
          return keep
    inl stm = return stm

-- | Turn sequences like `v0 = foo; v1 = v0; v2 = v1; return v2;` into a
--   straightforward `return foo;`.
--   Ignores LhsExp assignments, since we only introduce those when we actually
--   care about the assignment side effect.
inlineReturns :: JSTrav ast => ast -> TravM ast
inlineReturns ast = do
    (s, ast') <- foldMapJS (\_ _ -> True) pure2 foldRet S.empty ast
    mapM_ (flip putRef NullRet) $ S.toList s
    return ast'
  where
    pure2 s x = pure (s,x)
    foldRet s (Assign (NewVar _ lhs) rhs (Return (Var v))) | v == lhs = do
      return (s, Return rhs)
    foldRet s keep@(Assign (NewVar _ lhs) rhs (Jump (Shared lbl))) = do
      next <- getRef lbl
      case next of
        Return (Var v) | v == lhs ->
          return (S.insert lbl s, Return rhs)
        _ ->
          return (s, keep)
    foldRet s keep = do
      return (s, keep)

-- | Inline all occurrences of the given shared code path.
--   Use with caution - preferrably not at all!
inlineShared :: JSTrav ast => Lbl -> ast -> TravM ast
inlineShared lbl =
    mapJS (const True) pure inl
  where
    inl (Jump (Shared lbl')) | lbl == lbl' = getRef lbl
    inl s                                  = pure s

-- | Shrink case statements as much as possible.
shrinkCase :: JSTrav ast => ast -> TravM ast
shrinkCase =
    mapJS (const True) pure shrink
  where
    shrink (Case _ def [] next@(Shared lbl))
      | def == Jump next = getRef lbl
      | otherwise        = inlineShared lbl def
    shrink stm           = return stm

-- | Turn tail recursion on the given var into a loop, if possible.
--   Tail recursive functions that create closures turn into:
--   function f(a', b', c') {
--     while(1) {
--       var r = (function(a, b, c) {
--         a' = a; b' = b; c' = c;
--       })(a', b', c');
--       if(r != null) {
--         return r;
--       }
--     }
--   }
tailLoopify :: Var -> Exp -> TravM Exp
tailLoopify f fun@(Fun mname args body) = do
    tailrecs <- occurrences (not <$> isLambda) isTailRec body
    if tailrecs > Never
      then do
        needToCopy <- createsClosures body
        case needToCopy of
          True -> do
            let args' = map newName args
                ret = Return (Lit $ LNull)
            b <- mapJS (not <$> isLambda) pure (replaceByAssign ret args') body
            let (AST nullRetLbl _) = lblFor NullRet
                nn = newName f
                nv = NewVar False nn
                body' =
                  Forever $
                  Assign nv (Call 0 Fast (Fun Nothing args b) (map Var args'))$
                  Case (Var nn) (Return (Var nn)) [(Lit $ LNull, NullRet)] $
                  (Shared nullRetLbl)
            putRef nullRetLbl NullRet
            return $ Fun mname args' body'
          False -> do
            let c = Cont
            body' <- mapJS (not <$> isLambda) pure (replaceByAssign c args) body
            return $ Fun mname args (Forever body')
      else do
        return fun
  where
    isTailRec (Stm (Return (Call _ _ (Var f') _))) = f == f'
    isTailRec _                                    = False
    
    -- Only traverse until we find a closure
    createsClosures = foldJS (\acc _ -> not acc) isClosure False
    isClosure _ (Exp (Fun _ _ _)) = pure True
    isClosure acc _               = pure acc

    -- Assign any changed vars, then loop.
    replaceByAssign end as (Return (Call _ _ (Var f') as')) | f == f' = do
      let (first, second) = foldr assignUnlessEqual (id, end) (zip as as')
      return $ first second
    replaceByAssign _ _ stm =
      return stm

    -- Assign an expression to a variable, unless that expression happens to
    -- be the variable itself.
    assignUnlessEqual (v, (Var v')) (next, final) | v == v' =
      (next, final)
    assignUnlessEqual (v, x) (next, final) | any (x `contains`) args =
      (Assign (NewVar False (newName v)) x . next,
       Assign (LhsExp (Var v)) (Var $ newName v) final)
                                  | otherwise =
      (Assign (LhsExp (Var v)) x . next, final)
    
    newName (Internal (Name n mmod) _) =
      Internal (Name (' ':n) mmod) ""
    newName n =
      n
    
    contains (Var v) var          = v == var
    contains (Lit _) _            = False
    contains (Not x) var          = x `contains` var
    contains (BinOp _ a b) var    = a `contains` var || b `contains` var
    contains (Fun _ _ _) _        = False
    contains (Call _ _ f' xs) var = f' `contains` var||any (`contains` var) xs
    contains (Index a i) var      = a `contains` var || i `contains` var
    contains (Arr xs) var         = any (`contains` var) xs
    contains (AssignEx l r) var   = l `contains` var || r `contains` var
    contains (IfEx c t e) var     = any (`contains` var) [c,t,e]
tailLoopify _ fun = do
  return fun
