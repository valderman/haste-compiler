-- | Transformations purely on the JS AST, generally in the interest of
--   producing smaller, faster or more readable code.
module Haste.Optimize (optCase, optApp) where
import Haste.AST as AST
import Haste.Monad

-- | Generate an optimized switch statement. For a wonder, this code is almost
--   always MORE readable than its unoptimized equivalent.
--   Note that bad things happen if simplifyAlts is NOT called on a switch
--   statement, as PolyAlt alttypes and the like will only work when the
--   comparison is eliminated (there can be only one alt when there are
--   polyalts involved.)
optCase :: (JSExp -> JSExp) -> JSVar -> JSVar -> [JSAlt] -> JSGen cfg JSExp
optCase cmp scrut result alts = do
  case simplifyAlts cmp scrut alts of
    Left stmt -> do
      case ifToTernary stmt of
        Just tExp -> return tExp
        _         -> emit stmt >> return (AST.Var result)
    Right alts' -> do
      emit $ Case (cmp $ AST.Var scrut) alts'
      return $ AST.Var result

-- | Optimize a function application. Again, this is more readable than not
--   optimizing.
optApp :: Int -> JSExp -> JSExp
optApp arity = tryFastCall arity . foldUpApp

-- | Fold up nested function applications into a single call as far as
--   possible.
foldUpApp :: JSExp -> JSExp
foldUpApp (Call f as) =
  case foldUpApp f of
    Call f' as' -> Call f' (as' ++ as)
    f'          -> Call f' as
foldUpApp expr =
  expr

-- | Try to perform a fast call; that is, getting rid of the expensive
--   eval/apply machinery.
tryFastCall :: Int -> JSExp -> JSExp
tryFastCall arity (Call f as)
  | arity == length as =
    FastCall f as
tryFastCall _ app =
  app

-- | Turn if statements with single expression branches into expressions using
--   the ternary operator.
ifToTernary :: JSStmt -> Maybe JSExp
ifToTernary (If cond thenDo elseDo) = do
  then' <- getOkExpr thenDo
  else' <- getOkExpr elseDo
  return $ IfExp cond then' else'
  where
    getOkExpr [NewVar _ ex] = Just ex
    getOkExpr _             = Nothing
ifToTernary _ =
  Nothing

-- | Turn a few common switch statements into smaller code constructs.
--   These transformations rely on the fact that a Core case expression must
--   always be complete and that its alternatives may not overlap.
--   This means that if we have two alternatives, if the first one does not
--   match, the second one will.
simplifyAlts :: (JSExp -> JSExp) -> JSVar -> [JSAlt] -> Either JSStmt [JSAlt]
simplifyAlts cmp scrut as =
  case as of
    -- Core requires DEFAULT alts to come first, but we want them last in JS.
    (a'@(Def _):as'@(_:_)) ->
      simplifyAlts cmp scrut (as' ++ [a'])

    -- Turn any false/anything comparisons into if statements.
    -- As case alts are ordered ascending, false will always come first.
    [Cond (AST.Lit (Boolean False)) ifFalseDo, ifTrueDo] ->
      Left $ If (cmp (Var scrut)) (getStmts ifTrueDo) ifFalseDo
    [Cond (AST.Lit (Num 0)) ifFalseDo, ifTrueDo] ->
      Left $ If (cmp $ Var scrut) (getStmts ifTrueDo) ifFalseDo
    
    -- Turn any two-alt switch statements into if/then/else.
    [Cond cond thenDo, elseDo] ->
      Left $ If (BinOp Eq (cmp $ Var scrut) cond)
                thenDo
                (getStmts elseDo)

    -- No interesting transformations to make
    _ ->
      Right as
  where
    getStmts (Def ss) = ss
    getStmts (Cond _ ss) = ss
