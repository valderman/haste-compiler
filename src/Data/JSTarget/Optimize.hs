{-# LANGUAGE PatternGuards, TupleSections, DoAndIfThenElse, OverloadedStrings #-}
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
import qualified Data.ByteString.Char8 as BS

-- | Turn tail recursion into loops.
fixTailCalls :: Var -> Exp -> TravM Exp
fixTailCalls fun ast = do
    ast' <- assignToSubst ast >>= tailLoopify fun
    mapJS (const True) pure loopify ast'
  where
    loopify (Assign lhs@(NewVar _ f) body next) =
      Assign lhs <$> (assignToSubst body >>= tailLoopify f) <*> pure next
    loopify stm =
      return stm

-- TODO: tryTernary may inline calls that would otherwise be in tail position
--       which is something we'd really like to avoid.
optimizeFun :: Var -> Exp -> Exp
optimizeFun f ast =
  runTravM $ do
    shrinkCase ast
    >>= inlineAssigns
    >>= optimizeArrays
    >>= inlineReturns
    >>= zapJSStringConversions
    >>= optimizeThunks
    >>= optimizeArrays
    >>= fixTailCalls f
    >>= inlineShortJumpTailcall
    >>= trampoline
    >>= ifReturnToTernary
    >>= smallStepInline
    >>= inlineJSPrimitives

topLevelInline :: Stm -> Stm
topLevelInline ast =
  runTravM $ do
    unTrampoline ast
    >>= unevalLits
    >>= inlineIntoEval
    >>= inlineAssigns
    >>= optimizeArrays
    >>= optimizeThunks
    >>= smallStepInline
    >>= optimizeArrays
    >>= zapJSStringConversions

-- | Attempt to turn two case branches into a ternary operator expression.
tryTernary :: Var
           -> Exp
           -> Exp
           -> (Stm -> Stm)
           -> [(Exp, Stm -> Stm)]
           -> Maybe Exp
tryTernary self scrut retEx def [(m, alt)] =
    runTravM opt
  where
    selfOccurs (Exp (Var v) _) = v == self
    selfOccurs _               = False
    def' = def $ Return retEx
    alt' = alt $ Return retEx
    opt = do
      -- Make sure the return expression is used somewhere, then cut away all
      -- useless assignments. If what's left is a single Return statement,
      -- we have a pure expression suitable for use with ?:.
      def'' <- inlineAssignsLocal def'
      alt'' <- inlineAssignsLocal alt'
      -- If self occurs in either branch, we can't inline or we risk ruining
      -- tail call elimination.
      selfInDef <- occurrences (const True) selfOccurs def''
      selfInAlt <- occurrences (const True) selfOccurs alt''
      case (selfInDef + selfInAlt, def'', alt'') of
        (Never, Return el, Return th) ->
          return $ Just $ IfEx (BinOp Eq scrut m) th el
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

    appearsLHS ex =
      foldJS (\x _ -> not x) (\x s -> return $ x || ex `isLHSOf` s) False

    -- Make an exception for expressions of the form @x = E(x)@: since we know
    -- that @x@ is a literal and thus pointless to evaluate, we simply remove
    -- any such statements.
    isLHSOf v (Stm stm _) | isEvalUpd (==v) stm        = False
    isLHSOf v (Stm (Assign (LhsExp _ (Var v')) _ _) _) = v == v'
    isLHSOf v (Exp (AssignEx (Var v') _) _)            = v == v'
    isLHSOf _ _                                        = False

    inl m keep@(Assign l ex next)
      -- Inline all non-string literals l which do not appear at the LHS of an
      -- assignment. Thunk updates of the form @x = E(x)@ where @x == l@
      -- don't count as a proper LHS occurrence, and are removed outright
      -- instead since a literal is guaranteed to never be a thunk.
      | Just lhs <- inlinableAssignLHS l, Lit x <- ex, not (stringLit x) = do
        isLHS <- appearsLHS lhs next
        if isLHS
          then do
            return keep
          else do
            next' <- mapJS (const True) pure (pure . removeUpdate (==lhs)) next
            replaceEx (const True) (Var lhs) ex next'
      | Just lhs <- inlinableAssignLHS l = do
        occursRec <- occurrences (const True) (varOccurs lhs) ex
        if occursRec == Never
          then do
            occursLocal <- occurrences isSafeForInlining (varOccurs lhs) next
            case M.lookup lhs m of
              Just Once | okToInline ex && occursLocal == Once -> do
                -- Inline any non-lambda, non-thunk, non JSLit value
                replaceEx isSafeForInlining (Var lhs) ex next
              _ | Lit _ <- ex -> do
                -- Inline any string literals provided that they don't appear
                -- more than once.
                occurs <- occurrences (const True) (varOccurs lhs) next
                if occurs == Once
                  then replaceEx (const True) (Var lhs) ex next
                  else return keep
              _ -> do
                return keep
          else do
            return keep
    inl _ stm = return stm

-- | Remove an occurrence of @ex = E(ex)@. Only call this for @ex@ which are
--   guaranteed to never be thunks.
removeUpdate :: (Var -> Bool) -> Stm -> Stm
removeUpdate p stm@(Assign _ _ next) | isEvalUpd p stm = next
removeUpdate _ stm                                     = stm

-- | Turn the common pattern @var x = e ; x = E(x)@ into @var x = E(e)@.
--   should run *after* 'unevalLits'.
inlineIntoEval :: JSTrav ast => ast -> TravM ast
inlineIntoEval ast = do
    mapJS (const True) pure (pure . inline) ast
  where
    inline (Assign l@(NewVar _ v) r s@(Assign _ _ next))
      | isEvalUpd (== v) s = Assign l (Eval r) next
    inline stm             = stm

isEvalUpd :: (Var -> Bool) -> Stm -> Bool
isEvalUpd p (Assign (LhsExp _ (Var v)) (Eval (Var v')) _) = p v && v == v'
isEvalUpd _ _                                             = False

stringLit :: Lit -> Bool
stringLit (LStr _) = True
stringLit _        = False

-- | Certain expressions are never OK to inline: lambdas, thunks and
--   JS literals (which are almost exclusively lambdas).
okToInline :: Exp -> Bool
okToInline (Fun {})   = False
okToInline (Thunk {}) = False
okToInline (JSLit {}) = False
okToInline _          = True

inlinableAssignLHS :: LHS -> Maybe Var
inlinableAssignLHS (NewVar True v)       = Just v
inlinableAssignLHS (LhsExp True (Var v)) = Just v
inlinableAssignLHS _                     = Nothing

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
--   1. A(thunk(return f), xs)
--        => A(f, xs)
--   2. thunk(x@(JSLit s)) | s is a JS function object or marked eager
--        => x
--   3. thunk(x@(Lit _))
--        => x
--   4. E(thunk(return x))
--        => x
--   5. E(x) | x is guaranteed to not be a thunk
--        => x
--
--   Note that #2 depends on the invariant of 'JSLit': a JS literal must not
--   perform side effects or significant computation.
optimizeThunks :: JSTrav ast => ast -> TravM ast
optimizeThunks ast =
    mapJS (const True) optEx return ast
  where
    optEx (Eval x)
      | Just x' <- fromThunkEx x           = return x'
      | definitelyNotThunk x               = return x
    optEx ex@(Thunk _ _)
      | Just l@(JSLit s) <- fromThunkEx ex =
        case maybeExtractStrict s of
          Just s'           -> return $ JSLit s'
          _ | isJSFunDecl s -> return l
            | otherwise     -> return ex
    optEx ex@(Thunk _ _)
      | Just l@(Lit _) <- fromThunkEx ex   = return l
    optEx (Call arity calltype f as)
      | Just f' <- fromThunkEx f           = return $ Call arity calltype f' as
    optEx ex                               = return ex

maybeExtractStrict :: BS.ByteString -> Maybe BS.ByteString
maybeExtractStrict js
  | "__strict(" `BS.isPrefixOf` js && ")" `BS.isSuffixOf` js =
    Just $ BS.init $ BS.drop 9 js
  | otherwise =
    Nothing

-- | Conservatively approximate whether a given JS literal is a function
--   declaration or not.
--
--   TODO: proper parsing here.
isJSFunDecl :: BS.ByteString -> Bool
isJSFunDecl s
  | "function(" `BS.isPrefixOf` s && "}" `BS.isSuffixOf` s  = True
  | "(function(" `BS.isPrefixOf` s && ")" `BS.isSuffixOf` s = True
  | otherwise                                               = False

-- | Unpack the given expression if it's a thunk.
fromThunk :: Exp -> Maybe Stm
fromThunk (Thunk _ body) = Just body
fromThunk _              = Nothing

-- | Unpack the given expression if it's a thunk without internal bindings.
fromThunkEx :: Exp -> Maybe Exp
fromThunkEx ex =
  case fromThunk ex of
    Just (Return ex')   -> Just ex'
    Just (ThunkRet ex') -> Just ex'
    _                   -> Nothing

-- | Gather a map of all inlinable symbols; that is, the ones that are used
--   exactly once.
gatherInlinable :: JSTrav ast => ast -> TravM (M.Map Var Occs)
gatherInlinable ast = do
    m <- foldJS (\_ _->True) countOccs (M.empty) ast
    return (M.filter (< Lots) m)
  where
    updVar (Just occs) = Just (occs+Once)
    updVar _           = Just Once
    updVarAss (Just o) = Just o
    updVarAss _        = Just Never

    {-# INLINE countOccs #-}
    countOccs m (Exp (Var v@(Internal _ _ _)) _) =
      pure (M.alter updVar v m)
    countOccs m (Stm (Assign (NewVar _ v) _ _) _) =
      pure (M.alter updVarAss v m)
    countOccs m (Stm (Assign (LhsExp True (Var v)) _ _) _) =
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
    countTCs s (Stm (Assign (LhsExp True (Var v)) (Fun _ body) _) _) = do
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

-- | Like 'inlineAssigns', but doesn't care what happens beyond a jump.
inlineAssignsLocal :: JSTrav ast => ast -> TravM ast
inlineAssignsLocal ast = do
    mapJS isSafeForInlining return inl ast
  where
    varOccurs lhs (Exp (Var lhs') _) = lhs == lhs'
    varOccurs _ _                    = False
    inl keep@(Assign l ex next) | Just lhs <- inlinableAssignLHS l = do
      occursRec <- occurrences (const True) (varOccurs lhs) ex
      case occursRec of
        Never -> do
          occurs <- occurrences (const True) (varOccurs lhs) next
          occursSafe <- occurrences isSafeForInlining (varOccurs lhs) next
          case (occurs, occursSafe) of
            (Never, Never) ->
              return (Assign blackHole ex next)
            _    | Fun _ _ <- ex -> do
              -- Don't inline lambdas at the moment.
              return keep
            (Once, Once) | Nothing <- fromThunk ex ->
              -- can't be recursive - inline
              replaceEx (not <$> isShared) (Var lhs) ex next
            _ ->
              -- Really nothing to be done here.
              return keep
        _ ->
          return keep
    inl stm = return stm

-- | Turn sequences like `v0 = foo; v1 = v0; v2 = v1; return v2;` into a
--   straightforward `return foo;`.
--   Ignores LhsExp assignments, since we only introduce those when we actually
--   care about the assignment side effect.
inlineReturns :: JSTrav ast => ast -> TravM ast
inlineReturns ast = do
    mapJS (const True) inl pure ast
  where
    inl (Fun as body)    = Fun as <$> go Nothing body
    inl (Thunk upd body) = Thunk upd <$> go Nothing body
    inl ex               = pure ex
 
    goAlt outside (ex, stm) = (ex,) <$> go outside stm

    go outside (Case c d as next) = do
      next' <- go outside next
      case returnLike next' of
        outside'@(Just _) ->
          Case c <$> go outside' d <*> mapM (goAlt outside') as <*> pure Stop
        _ ->
          Case c <$> go Nothing d <*> mapM (goAlt Nothing) as <*> pure next'
    go _ (Forever s) = do
      Forever <$> go Nothing s
    go outside (Assign l@(NewVar _ lhs) r next) = do
      next' <- go outside next
      case (next', outside) of
        (Stop, Just (Var v, ret))   | v == lhs -> return $ ret r
        (Return (Var v), _)         | v == lhs -> return $ Return r
        (ThunkRet (Var v), _)       | v == lhs -> return $ ThunkRet r
        (Assign ll (Var v) Stop, _) | v == lhs -> return $ Assign ll r Stop
        _                                      -> return $ Assign l r next'
    go outside (Assign l r next) = do
      Assign l r <$> go outside next
    go _ stm = do
      return stm

-- | Extract the expression returned from a Return of ThunkRet, as well as
--   a function to recreate that type of return.
returnLike :: Stm -> Maybe (Exp, Exp -> Stm)
returnLike (Return e)   = Just (e, Return)
returnLike (ThunkRet e) = Just (e, ThunkRet)
returnLike _            = Nothing

-- | Shrink case statements as much as possible.
shrinkCase :: JSTrav ast => ast -> TravM ast
shrinkCase =
    mapJS (const True) pure shrink
  where
    shrink (Case _ def [] next)
      | def == Stop = return next
      | otherwise   = replaceFinalStm next (== Stop) def
    shrink stm      = return stm

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
            let nn = newName f
                nv = NewVar False nn
                body' =
                  Forever $
                  Assign nv (Call 0 (Fast False) (Fun args b)
                                                 (map Var args')) $
                  Case (Var nn) (Return (Var nn)) [(Lit $ LNull, Stop)] $
                  Stop
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
    assignUnlessEqual (v, (Var v')) (next, final)
      | v == v' =
        (next, final)
    assignUnlessEqual (v, x) (next, final)
      | any (x `contains`) args =
        (Assign (NewVar False (newName v)) x . next,
         Assign (LhsExp False (Var v)) (Var $ newName v) final)
      | otherwise =
        (Assign (LhsExp False (Var v)) x . next, final)
    
    newName (Internal (Name n mmod) _ _) =
      Internal (Name (BS.cons ' ' n) mmod) "" True
    newName n =
      n
    
    contains (Var v) var          = v == var
    contains (Lit _) _            = False
    contains (JSLit _) _          = False
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
    go _ _           = error "zipAssign: different number of lhs and rhs!"

-- | Eliminate evaluation of vars that are guaranteed not to be thunks.
--   Mainly useful in 'topLevelInline'.
unevalLits :: JSTrav ast => ast -> TravM ast
unevalLits ast = do
    lits <- foldJS (\_ _ -> True) gatherLits S.empty ast
    mapJS (const True) pure (pure . removeUpdate (`S.member` lits)) ast
  where
    gatherLits s (Stm (Assign (NewVar _ v) rhs _) _)
      | definitelyNotThunk rhs         = pure $ S.insert v s
      | Var v' <- rhs, v' `S.member` s = pure $ S.insert v s
    gatherLits s _                     = pure s

-- | Inline calls to JS @eval@, @__set@, @__get@ and @__has@ and apply
--   functions for "Haste.Foreign".
inlineJSPrimitives :: JSTrav ast => ast -> TravM ast
inlineJSPrimitives =
    inlineFuns >=> optimizeThunks
  where
    inlineFuns = mapJS (const True) (return . inl) return
    inl ex@(Call _ (Fast _) (Var (Foreign fn)) args) =
      case (fn, args) of
        ("eval", [Lit (LStr s)]) -> JSLit s
        ("__app0", [f])          -> Call 0 (Fast False) f []
        ("__app1", f:xs)         -> Call 0 (Fast False) f xs
        ("__app2", f:xs)         -> Call 0 (Fast False) f xs
        ("__app3", f:xs)         -> Call 0 (Fast False) f xs
        ("__app4", f:xs)         -> Call 0 (Fast False) f xs
        ("__app5", f:xs)         -> Call 0 (Fast False) f xs
        ("__get", [o, k])        -> Index o k
        ("__set", [o, k, v])     -> AssignEx (Index o k) v
        ("__has", [o, k])        -> BinOp Neq (Index o k) (JSLit "undefined")
        _                        -> ex
    inl ex =
      ex

-- | Turn all assignments of the form @var v1 = e ; exp@ into @exp[v1/e]@,
--   provided that @v1@ is not used as a known location and @e@ is either a
--   variable or a non-string literal.
--   Also does not inline vars into lambdas.
--   Should go before 'tailLoopify'.
assignToSubst :: JSTrav ast => ast -> TravM ast
assignToSubst ast = do
    mapJS (const True) return inl ast
  where
    inl stm@(Assign (NewVar _ v) x next) | not (isKnownLoc v) = do
      case x of
        (Var _)        -> do
          -- TODO: this can be replaced by a map (to replace) and a count of
          --       remaining occurrences of v (fold)
          (c, stm') <- replaceExWithCount (isSafeForInlining) (Var v) x next
          (c', _) <- replaceExWithCount (pure True) (Var v) x next
          if c == c'
            then return stm' -- No occurrences inside lambda
            else return stm
        (Lit (LStr _)) -> return stm
        (Lit _)        -> replaceEx (pure True) (Var v) x next
        _              -> return stm
    inl stm = do
      return stm

-- | Perform various trivially correct local inlinings:
--   var x = e; return [0, e] (boxing at the end of a thunk/function)
--    => [0, e]
--   thunk(x) where x is non-computing and non-recursive
--     => x
smallStepInline :: JSTrav ast => ast -> TravM ast
smallStepInline ast = do
    mapJS (const True) return inl ast
  where
    inl (Assign (NewVar _ v) ex (Return (Arr [l@(Lit _), Var v'])))
      | v == v' =
        return (Return (Arr [l, ex]))
    inl (Assign (NewVar _ v) ex (ThunkRet (Arr [l@(Lit _), Var v'])))
      | v == v' =
        return (ThunkRet (Arr [l, ex]))
    -- Unpack thunks which don't provide actual laziness.
    inl (Assign lhs@(NewVar _ v) t@(Thunk _ _) next)
      | Just ex <- fromThunkEx t, safeToUnThunk ex = do
        case ex of
          Thunk _ _ -> return $ Assign lhs ex next
          _         -> Assign lhs ex <$> eliminateEvalOf v next
    -- Merge @a = eval(a) ; a = eval(a)@ into a single eval.
    inl stm@(Assign (LhsExp _ (Var v1)) (Eval (Var v1')) next)
      | v1 == v1' = do
        case next of
          stm'@(Assign (LhsExp _ (Var v2)) (Eval (Var v2')) _)
            | v1 == v2 && v1' == v2' -> do
              return stm'
          _ -> do
            return stm
    inl stm =
      return stm

-- | Eliminate elimination of a variable. Only use when  *absolutely certain*
--   that the variable can never be a thunk.
eliminateEvalOf :: JSTrav ast => Var -> ast -> TravM ast
eliminateEvalOf v ast = mapJS (const True) return elim ast
  where
    elim (Assign (LhsExp _ (Var v')) (Eval (Var v'')) next)
      | v' == v'' && v == v' =
        return next
    elim stm =
        return stm

-- | Is the given expression safe to extract from a thunk?
--   An expression is safe to unthunk iff evaluating it will not cause
--   computation to take place or a variable do be dereferenced.
safeToUnThunk :: Exp -> Bool
safeToUnThunk ex =
  case ex of
    Lit _      -> True
    JSLit l    -> isJSFunDecl l
    Fun _ _    -> True
    Thunk _ _  -> True
    Arr arr    -> all safeToUnThunk arr
    _          -> False
