{-# LANGUAGE PatternGuards #-}
-- | Optimizations over the JSTarget AST.
module Data.JSTarget.Optimize (
    optimizeCase, optimizeFun, tryTernary
  ) where
import Data.JSTarget.AST
import Data.JSTarget.Op
import Data.JSTarget.Traversal
import Control.Applicative
import Data.List (foldl')

optimizeFun :: Var -> AST Exp -> AST Exp
optimizeFun f (AST ex js) =
  runTravM (tailLoopify f ex) js

optimizeCase :: JSTrav ast => AST ast -> AST ast
optimizeCase (AST ast js) =
  runTravM (shrinkCase ast >>= inlineReturns >>= inlineAssigns) js

-- | Attempt to turn two case branches into a ternary operator expression.
tryTernary :: AST Exp
           -> AST Exp
           -> (AST Stm -> AST Stm)
           -> [(AST Exp, AST Stm -> AST Stm)]
           -> Maybe (AST Exp)
tryTernary scrut retEx def [(m, alt)] =
    case runTravM opt allJumps of
      AST (Just ex) js -> Just (AST ex js)
      _                -> Nothing
  where
    def' = def $ Return <$> retEx
    alt' = alt $ Return <$> retEx
    AST _ allJumps = scrut >> m >> def' >> alt'
    opt = do
      -- Make sure the return expression is used somewhere, then cut away all
      -- useless assignments. If what's left is a single Return statement,
      -- we have a pure expression suitable for use with ?:.
      def'' <- inlineAssignsLocal $ astCode def'
      alt'' <- inlineAssignsLocal $ astCode alt'
      case (def'', alt'') of
        (Return el, Return th) ->
          return $ Just $ IfEx (BinOp Eq (astCode scrut) (astCode m)) th el
        _ ->
          return Nothing
tryTernary _ _ _ _ =
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
replaceEx :: JSTrav ast => Exp -> Exp -> ast -> TravM ast
replaceEx old new =
  mapJS (not <$> isShared) (\x -> if x == old then pure new else pure x) pure

-- | Inline assignments where the assignee is only ever used once.
--   Does not inline anything into a shared code path, as that would break
--   things horribly.
--   Ignores LhsExp assignments, since we only introduce those when we actually
--   care about the assignment side effect.
inlineAssigns :: JSTrav ast => ast -> TravM ast
inlineAssigns ast = do
    mapJS (const True) return inl ast
  where
    always = const True
    noJump = not <$> isShared
    varOccurs lhs (Exp (Var lhs')) = lhs == lhs'
    varOccurs _ _                  = False
    inl keep@(Assign (NewVar lhs) ex next) = do
      occurs <- occurrences always (varOccurs lhs) next
      occursLocal <- occurrences noJump (varOccurs lhs) next
      if occurs == occursLocal
        then case occurs of
          Never -> replaceEx (Var lhs) ex next -- return next
          Once  -> replaceEx (Var lhs) ex next
          Lots  -> return keep
        else return keep
    inl stm = return stm

-- | Like `inlineAssigns`, but doesn't care what happens beyond a jump.
inlineAssignsLocal :: JSTrav ast => ast -> TravM ast
inlineAssignsLocal ast = do
    mapJS (\n -> not (isLambda n || isShared n)) return inl ast
  where
    varOccurs lhs (Exp (Var lhs')) = lhs == lhs'
    varOccurs _ _                  = False
    inl keep@(Assign (NewVar lhs) ex next) = do
      occurs <- occurrences (const True) (varOccurs lhs) next
      occurs' <- occurrences (const True) (varOccurs lhs) ex
      case occurs + occurs' of
        Never ->
          -- completely unnecessary - remove
          return next
        Once ->
          -- can't be recursive - inline
          replaceEx (Var lhs) ex next
        Lots | Fun Nothing vs body <- ex,
               occurs < Lots,
               Internal lhsname _ <- lhs ->
          -- only occurs at most once outside, so replace with named lambda
          -- since the binding is only relevant for recursion!
          replaceEx (Var lhs) (Fun (Just lhsname) vs body) next
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
    mapJS (const True) return inl ast
  where
    inl keep@(Assign (NewVar lhs) ex next) = do
      unchanged <- straightReturnPath (Var lhs) next
      if unchanged
        then replaceNullReturn (Return ex) next >> return (Return ex)
        else return keep
    inl stm = return stm

    replaceNullReturn r = mapJS (const True) return (rep r)
    rep r stm | stm == r = return NullRet
    rep _ stm            = return stm

-- | Is the given expression passed to a Return node unchanged
--   (modulo new name assignments)? If it is, we can get rid of the extra
--   assignments and return it right away.
--   Ignores LhsExp assignments, since we only introduce those when we actually
--   care about the assignment side effect.
straightReturnPath :: Exp -> Stm -> TravM Bool
straightReturnPath x (Return x') = do
  return $ x == x'
straightReturnPath x (Jump (Shared lbl)) = do
  getRef lbl >>= straightReturnPath x
straightReturnPath x (Assign (NewVar lhs) x' next) | x == x' = do
  straightReturnPath (Var lhs) next
straightReturnPath _ _ =
  return False

-- | Inline all occurrences of the given shared code path.
--   Use with caution - preferrably not at all!
inlineShared :: JSTrav ast => Lbl -> ast -> TravM ast
inlineShared lbl =
    mapJS (not <$> isShared) pure inl
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
--   TODO: optimize tail recursive functions that create closures into:
--   function f(a', b', c') {
--     while(1) {
--       (function(a, b, c) {
--         a' = a; b' = b; c' = c;
--       })(a', b', c');
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
            return fun -- TODO: add optimization here as well!
          False -> do
            body' <- mapJS (not <$> isLambda) pure replaceByAssign body
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
    replaceByAssign (Return (Call _ _ (Var f') args')) | f == f' = do
      return $ foldl' assignUnlessEqual Cont (zip args args')
    replaceByAssign stm =
      return stm

    -- Assign an expression to a variable, unless that expression happens to
    -- be the variable itself.
    assignUnlessEqual next (v, (Var v')) | v == v' =
      next
    assignUnlessEqual next (v, x) =
      Assign (LhsExp (Var v)) x next
tailLoopify _ fun = do
  return fun
