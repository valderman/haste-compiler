-- | Optimizations over the JSTarget AST.
module Data.JSTarget.Optimize (
    inlineAssigns, inlineReturns, shrinkCase, tailLoopify
  ) where
import Data.JSTarget.AST
import Data.JSTarget.Op
import Data.JSTarget.Traversal
import Control.Applicative
import Data.List (foldl')

-- | How many times does an expression satisfying the given predicate occur in
--   an AST (including jumps)?
occurrences :: JSTrav ast => (ASTNode -> Bool) -> ast -> AST Occs
occurrences p ast =
    foldJS trav count Never ast
  where
    trav n _ = n < Lots -- Stop traversal if we're already >1.
    count n ast | p ast = pure $ n + Once
    count n _           = pure n

-- | Replace all occurrences of an expression, without entering shared code
--   paths. IO ordering is preserved even when entering lambdas thanks to
--   State# RealWorld.
replaceEx :: JSTrav ast => Exp -> Exp -> ast -> AST ast
replaceEx old new =
  mapJS (not <$> isShared) (\x -> if x == old then pure new else pure x) pure

-- | Inline assignments where the assignee is only ever used once.
--   Does not inline anything into a shared code path, as that would break
--   things horribly.
--   Ignores LhsExp assignments, since we only introduce those when we actually
--   care about the assignment side effect.
inlineAssigns :: JSTrav ast => ast -> AST ast
inlineAssigns ast = do
    mapJS (const True) return inl ast
  where
    inl keep@(Assign (NewVar lhs) ex next) = do
      occurs <- occurrences (\(Exp (Var lhs')) -> lhs == lhs') next
      case occurs of
        Never -> return next
        Once  -> replaceEx (Var lhs) ex next
        Lots  -> return keep
    inl stm = return stm

-- | Turn sequences like `v0 = foo; v1 = v0; v2 = v1; return v2;` into a
--   straightforward `return foo;`.
--   Ignores LhsExp assignments, since we only introduce those when we actually
--   care about the assignment side effect.
inlineReturns :: JSTrav ast => ast -> AST ast
inlineReturns ast = do
    mapJS (const True) return inl ast
  where
    inl keep@(Assign (NewVar lhs) ex next) = do
      unchanged <- straightReturnPath (Var lhs) next
      if unchanged
        then return $ Return ex
        else return keep
    inl stm = return stm

-- | Is the given expression passed to a Return node unchanged
--   (modulo new name assignments)? If it is, we can get rid of the extra
--   assignments and return it right away.
--   Ignores LhsExp assignments, since we only introduce those when we actually
--   care about the assignment side effect.
straightReturnPath :: Exp -> Stm -> AST Bool
straightReturnPath x (Return x') = do
  return $ x == x'
straightReturnPath x (Jump (Shared lbl)) = do
  getRef lbl >>= straightReturnPath x
straightReturnPath x (Assign (NewVar lhs) x' next) | x == x' = do
  straightReturnPath (Var lhs) next

-- | Inline all occurrences of the given shared code path.
--   Use with caution - preferrably not at all!
inlineShared :: JSTrav ast => Lbl -> ast -> AST ast
inlineShared lbl =
    mapJS (not <$> isShared) pure inl
  where
    inl (Jump (Shared lbl')) | lbl == lbl' = getRef lbl
    inl s                                  = pure s

-- | Shrink case statements as much as possible.
shrinkCase :: JSTrav ast => ast -> AST ast
shrinkCase =
    mapJS (const True) pure shrink
  where
    shrink (Case cond def [] next@(Shared lbl))
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
tailLoopify :: Var -> Exp -> AST Exp
tailLoopify f fun@(Fun args body) = do
    tailrecs <- occurrences isTailRec body
    if tailrecs > Never
      then do
        needToCopy <- createsClosures body
        case needToCopy of
          True -> do
            return fun -- TODO: add optimization here as well!
          False -> do
            body' <- mapJS (not <$> isLambda) pure replaceByAssign body
            return $ Fun args (Forever body')
      else do
        return fun
  where
    isTailRec (Stm (Return (Call _ _ (Var f') _))) = f == f'
    isTailRec _                                    = False
    
    -- Only traverse until we find a closure
    createsClosures = foldJS (\acc _ -> not acc) isClosure False
    isClosure _ (Exp (Fun _ _)) = pure True
    isClosure acc _             = pure acc

    -- Assign any changed vars, then loop.
    replaceByAssign (Return (Call _ _ (Var f') args')) | f == f' = do
      return $ foldl' assignUnlessEqual Cont (zip args args')

    -- Assign an expression to a variable, unless that expression happens to
    -- be the variable itself.
    assignUnlessEqual next (v, (Var v')) | v == v' =
      next
    assignUnlessEqual next (v, x) =
      Assign (LhsExp (Var v)) x next
tailLoopify _ fun = do
  return fun
