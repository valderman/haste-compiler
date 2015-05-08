{-# LANGUAGE PatternGuards, TupleSections, DoAndIfThenElse #-}
-- | Optimizations over the JSTarget AST.
module Data.JSTarget.Optimize (
    optimizeFun, tryTernary, topLevelInline
  ) where
import Data.JSTarget.AST
import Data.JSTarget.Op
import Data.JSTarget.Traversal
import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S

-- TODO: tryTernary may inline calls that would otherwise be in tail position
--       which is something we'd really like to avoid.

optimizeFun :: Var -> AST Exp -> AST Exp
optimizeFun f (AST ast js) =
  flip runTravM js $ do
    shrinkCase ast
--    >>= inlineOldVars
--    >>= zapDoubleAssigns
--    >>= zapUselessAssigns - Not quite correct yet; disable for now.
    >>= inlineAssigns
    >>= optimizeArrays
    >>= inlineReturns
    >>= zapJSStringConversions
    >>= optimizeThunks
    >>= optimizeArrays
    >>= tailLoopify f
    >>= inlineShortJumpTailcall
    >>= trampoline
    >>= ifReturnToTernary
    >>= inlineEval

topLevelInline :: AST Stm -> AST Stm
topLevelInline (AST ast js) =
  flip runTravM js $ do
    unTrampoline ast
    >>= inlineAssigns
    >>= optimizeArrays
    >>= optimizeThunks
    >>= optimizeArrays
    >>= zapJSStringConversions

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
    selfOccurs (Exp (Var v) _) = v == self
    selfOccurs _               = False
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

-- | Inline assignments where the assignee is only ever used once.
--   Does not inline anything into a shared code path, as that would break
--   things horribly.
--   Ignores LhsExp assignments, since we only introduce those when we actually
--   care about the assignment side effect.
--
--   Note: a thunk may ONLY be inlined into a lambda if it performs no useful
--         work, to avoid computing expensive thunks more than once.
inlineAssigns :: JSTrav ast => ast -> TravM ast
inlineAssigns ast = do
    inlinable <- gatherInlinable ast
    mapJS (const True) return (inl inlinable) ast
  where
    varOccurs lhs (Exp (Var lhs') _) = lhs == lhs'
    varOccurs _ _                    = False
    inl m keep@(Assign l ex next) | Just lhs <- inlinableAssignLHS l = do
      occursRec <- occurrences (const True) (varOccurs lhs) ex
      if occursRec == Never
        then do
          occursLocal <- occurrences (not <$> isShared) (varOccurs lhs) next
          case M.lookup lhs m of
            Just occ | occ == occursLocal ->
              case occ of
                -- Don't inline lambdas currently.
                _    | Fun vs body <- ex -> do
                  return keep
                -- Inline of any non-lambda value
                Once | Nothing <- fromThunk ex -> do
                  replaceEx (not <$> isShared) (Var lhs) ex next
                _ -> do
                  return keep
            _ ->
              return keep
        else do
          return keep
    inl _ stm = return stm

inlinableAssignLHS :: LHS -> Maybe Var
inlinableAssignLHS (NewVar True v) = Just v
inlinableAssignLHS (OldVar True v) = Just v
inlinableAssignLHS _               = Nothing

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
    opt (Call _ _ (Var (Foreign "toJSStr")) [
           Call _ _ (Var (Foreign "unCStr")) [x]]) =
      return x
    opt (Call _ _ (Var (Foreign "toJSStr")) [
           Eval (Call _ _ (Var (Foreign "unCStr")) [x])]) =
      return x
    opt (Thunk _ (Return x@(Call _ _ (Var (Foreign "unCStr")) [Lit _]))) =
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
--   thunk(x) where x is non-computing and non-recursive
--     => x
--
--   TODO: figure out efficient way to only perform the 4th optimization when x
--         is not recursive.
optimizeThunks :: JSTrav ast => ast -> TravM ast
optimizeThunks ast =
    mapJS (const True) optEx return ast
  where
    optEx (Eval x)
      | Just x' <- fromThunkEx x = return x'
      | Fun _ _ <- x             = return x
    optEx (Call arity calltype f args) | Just f' <- fromThunkEx f =
      return $ Call arity calltype f' args
--    optEx ex | Just ex' <- fromThunkEx ex, not (computingEx ex') =
--      return ex'
    optEx ex =
      return ex

-- | Unpack the given expression if it's a thunk.
fromThunk :: Exp -> Maybe Stm
fromThunk (Thunk _ body) = Just body
fromThunk _              = Nothing

-- | Unpack the given expression if it's a thunk without internal bindings.
fromThunkEx :: Exp -> Maybe Exp
fromThunkEx ex =
  case fromThunk ex of
    Just (Return ex') -> Just ex'
    _                 -> Nothing

-- | Is the given expression a thunk which when evaluated performs some kind of
--   computation?
computingThunk :: Exp -> Bool
computingThunk e =
  case fromThunkEx e of
    Just e' -> computingEx e'
    _       -> False

-- | Does the given expression compute something? An expression is
--   non-computing if it is a variable, a literal, a lambda abstraction,
--   a thunk or an array which only has non-computing elements.
computingEx :: Exp -> Bool
computingEx ex =
  case ex of
    Var _      -> False
    Lit _      -> False
    Fun _ _    -> False
    Thunk _ _  -> False
    Arr arr    -> any computingEx arr
    _          -> True


-- | Gather a map of all inlinable symbols; that is, the ones that are used
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
    countOccs m (Exp (Var v@(Internal _ _)) _) =
      pure (M.alter updVar v m)
    countOccs m (Stm (Assign (NewVar _ v) _ _) _) =
      pure (M.alter updVarAss v m)
    countOccs m (Stm (Assign (OldVar _ v) ex _) _) =
      pure (M.alter updVarAss v m)
    countOccs m _ =
      pure m

-- | May the given expression ever tailcall?
--   TODO:
--     Be slightly smarter about handling locally defined functions; always
--     counting a tailcall from a local as a tailcall from the containing
--     function seems a bit too restrictive. On the other hand, this makes
--     only a very slight difference in the number of unnecessary tailcalls
--     eliminated.
mayTailcall :: JSTrav ast => ast -> TravM Bool
mayTailcall ast = do
  foldJS enter countTCs False ast
  where
    enter True _                = False
    enter _ (Exp (Thunk _ _) _) = False
--    enter _ (Exp (Fun _ _) _)   = False
    enter _ _                   = True
    countTCs _ (Stm (Tailcall _) _) = return True
    countTCs acc _                  = return acc

-- | Gather a map of all symbols which we know will never make tail calls.
--   All calls to functions in this set can then safely be de-trampolined.
gatherNonTailcalling :: Stm -> TravM (S.Set Var)
gatherNonTailcalling stm = do
    foldJS (\_ _ -> True) countTCs S.empty stm
  where
    countTCs s (Exp (Var v@(Foreign _)) _) = do
      return $ S.insert v s
    countTCs s (Stm (Assign (NewVar _ v) (Fun _ body) _) _) = do
      tc <- mayTailcall body
      return $ if not tc then S.insert v s else s      
    countTCs s (Stm (Assign (OldVar _ v) (Fun _ body) _) _) = do
      tc <- mayTailcall body
      return $ if not tc then S.insert v s else s
    countTCs s _ = do
      return s

-- | Remove trampolines wherever possible.
--   The trampoline machinery has some overhead; two extra activation records
--   on the stack for a single, non-tailcalling function, to be precise.
--   We observe that bouncing a function that is guaranteed to never tailcall
--   is a waste of resources, so we can remove those bounces.
--   Additionally, tailcalling a function which is guaranteed to not tailcall
--   in turn is wasteful (see above comment about overhead), so we can
--   eliminate any such function.
--   Since the tailcalling machinery grows the stack by a total of three
--   activation records for an arbitrary string of tailcalling functions,
--   we can apply this procedure recursively three times and still be
--   guaranteed to use no more stack frames than we would have without this
--   optimization.
unTrampoline :: Stm -> TravM Stm
unTrampoline = go >=> go >=> go
  where
    go s = do
      ntcs <- gatherNonTailcalling s
      mapJS (const True) (unTr ntcs) (unTC ntcs) s

    unTr ntcs (Call ar (Normal True) f@(Var v) xs)
      | v `S.member` ntcs =
        return $ Call ar (Normal False) f xs
    unTr ntcs (Call ar (Fast True) f@(Var v) xs)
      | v `S.member` ntcs =
        return $ Call ar (Fast False) f xs
    unTr _ c@(Call ar (Normal True) f@(Fun _ body) xs) = do
        tc <- mayTailcall body
        return $ if tc then c else Call ar (Normal False) f xs
    unTr _ c@(Call ar (Fast True) f@(Fun _ body) xs) = do
        tc <- mayTailcall body
        return $ if tc then c else Call ar (Fast False) f xs
    unTr _ x =
        return x

    -- If we know for certain that the function we're tailcalling will not
    -- tailcall in turn we should not tailcall it, since that would mean two
    -- activation records on the stack - one for the trampoline and one for
    -- the function itself.
    unTC ntcs (Tailcall c@(Call _ _ (Var v) _))
      | v `S.member` ntcs =
        return $ Return c
    unTC _ tc@(Tailcall c@(Call _ _ (Fun _ body) _)) = do
        maytc <- mayTailcall body
        if not maytc then return (Return c) else return tc
    unTC _ x =
        return x

-- | Inline all reorderable x = ... into the next occurrence of x outside of a
--   lambda, keeping the assignment.
inlineOldVars :: JSTrav ast => ast -> TravM ast
inlineOldVars ast = do
    mapJS (const True) return inl ast
  where
    inl (Assign (OldVar True v) rhs next) =
      replaceFirst enter (Var v) (AssignEx (Var v) rhs) next
    inl stm =
      return stm
    enter = not . (isLambda .|. isConditional)

-- | Turn all @var x = y = z@ into @y = z@.
zapDoubleAssigns :: JSTrav ast => ast -> TravM ast
zapDoubleAssigns ast = do
    mapJS (const True) return inl ast
  where
    inl (Assign (NewVar True v) (AssignEx (Var v') rhs) next) = do
      next' <- replaceEx (const True) (Var v) (Var v') next
      return $ Assign (OldVar True v') rhs next'
    inl stm =
      return stm

-- | Turn all @x = y@ where @x@ is never used into @y@.
--   TODO: use this as basis for smarter inlining?
--   TODO: not quite correct yet, so disabled. Fix ASAP!
zapUselessAssigns :: JSTrav ast => ast -> TravM ast
zapUselessAssigns ast = do
    mapJS (const True) doLam return ast
  where
    inl occs keep@(AssignEx (Var v) rhs) = do
      case M.lookup v occs of
        Just n
          | n < 1     -> return rhs
          | otherwise -> return keep
    inl _ stm =
      return stm

    withLam (Fun as body) f    = Fun as <$> f body
    withLam (Thunk upd body) f = Thunk upd <$> f body
    withLam ex _               = return ex
    doLam ex = withLam ex $ \body -> do
      occs <- foldJS (\_ _ -> True) countOccs M.empty body
      mapJS (const True) (inl occs) return body

    -- Subtract non-lambda recursive occurrences and the assignment occurrence.
    countOccs m (Exp (AssignEx (Var v) rhs) _) = do
      rhsOccs <- nonLamOccs (equals $ Var v) rhs
      return $ M.alter (subtr (rhsOccs+1)) v m
    countOccs m (Exp (Var v) _) = do
      return $ M.alter oneMore v m
    countOccs m _ = do
      return m

    equals ex (Exp ex' _) = ex == ex'
    equals _ _            = False

    subtr s Nothing  = Just $ 0-s
    subtr s (Just n) = Just $ n-s

    oneMore Nothing  = Just 1
    oneMore (Just n) = Just (n+1)

    nonLamOccs pred = foldJS (const $ not . isLambda) (count pred) 0
    count p n node
      | p node    = pure $ n + 1
      | otherwise = pure n

-- | Replace the first occurrence of an expression with another expression.
replaceFirst :: JSTrav ast => (ASTNode -> Bool) -> Exp -> Exp -> ast -> TravM ast
replaceFirst enter from to ast = do
    snd <$> foldMapJS enter' step (\found n -> return (found, n)) False ast
  where
    enter' True _   = False
    enter' _      n = enter n
    step found e
      | e == from = return (True, to)
      | otherwise = return (found, e)

-- | Like 'inlineAssigns', but doesn't care what happens beyond a jump.
inlineAssignsLocal :: JSTrav ast => ast -> TravM ast
inlineAssignsLocal ast = do
    mapJS (\n -> not (isLambda n || isShared n)) return inl ast
  where
    varOccurs lhs (Exp (Var lhs') _) = lhs == lhs'
    varOccurs _ _                    = False
    inl keep@(Assign l ex next) | Just lhs <- inlinableAssignLHS l = do
      occurs <- occurrences (const True) (varOccurs lhs) next
      occursRec <- occurrences (const True) (varOccurs lhs) ex
      case occurs + occursRec of
        Never ->
          return (Assign blackHole ex next)
        -- Don't inline lambdas at the moment.
        _     | Fun vs body <- ex -> do
          return keep
        Once | Nothing <- fromThunk ex ->
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
    foldRet s (Assign l rhs next)
      | Just (Var v, ret) <- returnLike next,
        Just (lhs,_) <- notLhsExp l,
        v == lhs = do
        return (s, ret rhs)
    foldRet s keep@(Assign l rhs (Jump (Shared lbl)))
      | Just (lhs, _) <- notLhsExp l = do
        next <- getRef lbl
        case returnLike next of
          Just (Var v, ret) | v == lhs ->
            return (S.insert lbl s, ret rhs)
          _ ->
            return (s, keep)
    foldRet s keep = do
      return (s, keep)

-- | Extract the expression returned from a Return of ThunkRet, as well as
--   a function to recreate that type of return.
returnLike :: Stm -> Maybe (Exp, Exp -> Stm)
returnLike (Return e)   = Just (e, Return)
returnLike (ThunkRet e) = Just (e, ThunkRet)
returnLike _            = Nothing

notLhsExp :: LHS -> Maybe (Var, Reorderable)
notLhsExp (NewVar r v) = Just (v, r)
notLhsExp (OldVar r v) = Just (v, r)
notLhsExp _            = Nothing

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

-- | Turn any calls in tail position into tailcalls.
--   Must run after @tailLoopify@ or we won't get loops for simple tail
--   recursive functions.
trampoline :: Exp -> TravM Exp
trampoline = mapJS (pure True) pure bounce
  where
    bounce (Return (Call arity call f args)) = do
      return $ Tailcall $ Call arity call' f args
      where
        call' =
          case call of
            Normal _ -> Normal False
            Fast _   -> Fast False
            c        -> c
    bounce s = do
      return s

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
tailLoopify f fun@(Fun args body) = do
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
                  Assign nv (Call 0 (Fast False) (Fun args b)
                                                 (map Var args')) $
                  Case (Var nn) (Return (Var nn)) [(Lit $ LNull, NullRet)] $
                  (Shared nullRetLbl)
            putRef nullRetLbl NullRet
            return $ Fun args' body'
          False -> do
            let c = Cont
            body' <- mapJS (not <$> isLambda) pure (replaceByAssign c args) body
            return $ Fun args (Forever body')
      else do
        return fun
  where
    isTailRec (Stm (Return (Call _ _ (Var f') _)) _) = f == f'
    isTailRec _                                      = False
    
    -- Only traverse until we find a closure
    createsClosures = foldJS (\acc _ -> not acc) isClosure False
    isClosure _ (Exp (Fun _ _) _)   = pure True
    isClosure _ (Exp (Thunk _ _) _) = pure True
    isClosure acc _                 = pure acc

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
    contains (Fun _ _) _          = False
    contains (Call _ _ f' xs) var = f' `contains` var||any (`contains` var) xs
    contains (Index a i) var      = a `contains` var || i `contains` var
    contains (Arr xs) var         = any (`contains` var) xs
    contains (AssignEx l r) var   = l `contains` var || r `contains` var
    contains (IfEx c t e) var     = any (`contains` var) [c,t,e]
    contains (Eval x) var         = x `contains` var
    contains (Thunk _ _) _        = False
tailLoopify _ fun = do
  return fun

-- | Inline a tailcalled function @f@ when:
--
--   * @f@ does not refer to itself; and
--   * @f@ is defined immediately before its call site.
--     (@let f = ... in tailcall f@)
--
--   Should be called *after* 'tailLoopify' but *before* trampoline for best
--   effect.
inlineShortJumpTailcall :: JSTrav ast => ast -> TravM ast
inlineShortJumpTailcall ast = do
    mapJS (const True) return inl ast
  where
    inl stm@(Assign (NewVar _ f) (Fun as b) tc)
      | Just (f', as') <- getTailcallInfo tc, f == f' = do
        occs <- occurrences (const True) (isEqualTo f) b
        case (occs, zipAssign (map (NewVar True) as) as' b) of
          (Never, Just b') -> return b'
          _                -> return stm
    inl stm =
      return stm
    isEqualTo v' (Exp (Var v) _) = v == v'
    isEqualTo _ _                = False

-- | Extract the function being called and its argument list from a
--   @Tailcall (Call ...)@ or @Return (Call ...)@, provided that the call is
--   completely saturated.
getTailcallInfo :: Stm -> Maybe (Var, [Exp])
getTailcallInfo (Tailcall (Call 0 _ (Var f) as)) = Just (f, as)
getTailcallInfo (Return (Call 0 _ (Var f) as))   = Just (f, as)
getTailcallInfo _                                = Nothing

-- | Assign several variables, before executing a statement.
zipAssign :: [LHS] -> [Exp] -> Stm -> Maybe Stm
zipAssign l r final
  | length l == length r = Just $ go l r
  | otherwise            = Nothing
  where
    go (v:vs) (x:xs) = Assign v x (go vs xs)
    go [] []         = final

-- | Inline calls to JS @eval@ function.
inlineEval :: JSTrav ast => ast -> TravM ast
inlineEval ast = do
    mapJS (const True) inl return ast
  where
    inl (Call _ (Fast _) (Var (Foreign "eval")) [Lit (LStr s)]) =
      return (JSLit s)
    inl exp =
      return exp
