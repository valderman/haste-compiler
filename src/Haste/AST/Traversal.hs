{-# LANGUAGE FlexibleInstances, TupleSections, PatternGuards, BangPatterns #-}
-- | Generic traversal of JSTarget AST types.
module Haste.AST.Traversal where
import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Haste.AST.Syntax

-- | AST nodes we'd like to fold and map over.
data ASTNode = Exp !Exp !Bool | Stm !Stm !Bool | Shared !Stm

type TravM a = Identity a

runTravM :: TravM a -> a
runTravM = runIdentity

class Show ast => JSTrav ast where
  -- | Bottom up transform over an AST.
  foldMapJS :: (a -> ASTNode -> Bool)       -- ^ Enter node?
            -> (a -> Exp -> TravM (a, Exp)) -- ^ Exp to Exp mapping.
            -> (a -> Stm -> TravM (a, Stm)) -- ^ Stm to Stm mapping.
            -> a                            -- ^ Starting accumulator.
            -> ast                          -- ^ AST to map over.
            -> TravM (a, ast)

  -- | Bottom up fold of an AST.
  foldJS :: (a -> ASTNode -> Bool)    -- ^ Should the given node be entered?
                                      --   The step function is always applied
                                      --   to the current node, however.
         -> (a -> ASTNode -> TravM a) -- ^ Step function.
         -> a                         -- ^ Initial value.
         -> ast                       -- ^ AST to fold over.
         -> TravM a

mapJS :: JSTrav ast
      => (ASTNode -> Bool)
      -> (Exp -> TravM Exp)
      -> (Stm -> TravM Stm)
      -> ast
      -> TravM ast
mapJS tr fe fs ast =
    snd <$> foldMapJS (const tr) (const' fe) (const' fs) () ast
  where
    {-# INLINE const' #-}
    const' f _ x = ((),) <$> f x

instance JSTrav a => JSTrav [a] where
  foldMapJS tr fe fs acc ast =
      go (acc, []) ast
    where
      go (a, xs') (x:xs) = do
        (a', x') <- foldMapJS tr fe fs a x
        go (a', x':xs') xs
      go (a, xs) _ = do
        return (a, reverse xs)
  foldJS tr f acc ast = foldM (foldJS tr f) acc ast

instance JSTrav Exp where
  foldMapJS tr fe fs = go
    where
      go acc ast
        | tr acc $! Exp ast False = do
          (acc', x) <- do
            case ast of
              v@(Var _)      -> pure (acc, v)
              l@(Lit _)      -> pure (acc, l)
              l@(JSLit _)    -> pure (acc, l)
              Not ex         -> fmap Not <$> go acc ex
              BinOp op a b   -> do
                (acc', a') <- go acc a
                (acc'', b') <- go acc' b
                return (acc'', BinOp op a' b')
              Fun vs stm     -> fmap (Fun vs) <$> foldMapJS tr fe fs acc stm
              Call ar c f xs -> do
                (acc', f') <- go acc f
                (acc'', xs') <- foldMapJS tr fe fs acc' xs
                return (acc'', Call ar c f' xs')
              Index arr ix   -> do
                (acc', arr') <- go acc arr
                (acc'', ix') <- go acc' ix
                return (acc'', Index arr' ix')
              Arr exs        -> fmap Arr <$> foldMapJS tr fe fs acc exs
              AssignEx l r   -> do
                (acc', l') <- go acc l
                (acc'', r') <- go acc' r
                return (acc'', AssignEx l' r')
              IfEx c th el   -> do
                (acc', c') <- go acc c
                (acc'', th') <- if tr acc (Exp th True)
                                  then go acc' th
                                  else return (acc', th)
                (acc''', el') <- if tr acc (Exp el True)
                                   then go acc'' el
                                   else return (acc'', el)
                return (acc''', IfEx c' th' el')
              Eval x         -> fmap Eval <$> go acc x
              Thunk upd x    -> fmap (Thunk upd) <$> foldMapJS tr fe fs acc x
          fe acc' x
        | otherwise = do
          fe acc ast
  
  foldJS tr f = go
    where
      go acc ast
        | tr acc $! expast = do
          flip f expast =<< do
            case ast of
              Var _           -> return acc
              Lit _           -> return acc
              JSLit _         -> return acc
              Not ex          -> go acc ex
              BinOp _ a b     -> go acc a >>= flip go b
              Fun _ stm       -> foldJS tr f acc stm
              Call _ _ fun xs -> go acc fun >>= flip (foldJS tr f) xs
              Index arr ix    -> go acc arr >>= flip go ix
              Arr exs         -> foldJS tr f acc exs
              AssignEx l r    -> go acc l >>= flip go r
              IfEx c th el    -> do
                acc' <- go acc c
                acc'' <- if tr acc $! Exp th True
                           then go acc' th
                           else return acc'
                if tr acc $! Exp th True
                  then go acc'' el
                  else return acc''
              Eval ex         -> go acc ex
              Thunk _upd stm  -> foldJS tr f acc stm
        | otherwise =
          f acc expast
        where !expast = Exp ast False

instance JSTrav Stm where
  foldMapJS tr fe fs = go
    where
      go acc ast
        | tr acc $! Stm ast False = do
          (acc', x) <- do
            case ast of
              Case ex def as nxt -> do
                (acc1, ex') <- foldMapJS tr fe fs acc ex
                (acc2, def') <- go acc1 def
                (acc3, as') <- foldMapJS tr fe fs acc2 as
                (acc4, nxt') <- if tr acc $! Shared nxt
                                  then go acc3 nxt
                                  else return (acc3, nxt)
                return (acc4, Case ex' def' as' nxt')
              Assign lhs ex next -> do
                (acc', lhs') <- foldMapJS tr fe fs acc lhs
                (acc'', ex') <- foldMapJS tr fe fs acc' ex
                (acc''', next') <- go acc'' next
                return (acc''', Assign lhs' ex' next')
              Forever stm        -> fmap Forever <$> go acc stm
              Return ex          -> fmap Return <$> foldMapJS tr fe fs acc ex
              Cont               -> return (acc, ast)
              Stop               -> return (acc, ast)
              Tailcall ex        -> fmap Tailcall <$> foldMapJS tr fe fs acc ex
              ThunkRet ex        -> fmap ThunkRet <$> foldMapJS tr fe fs acc ex
          fs acc' x
        | otherwise = do
          fs acc ast

  foldJS tr f = go
    where
      go acc ast
        | tr acc stmast = do
          flip f stmast =<< do
            case ast of
              Case ex def as nxt -> do
                acc' <- foldJS tr f acc ex >>= flip go def
                acc'' <- foldJS tr f acc' as
                if tr acc $! Shared nxt
                  then go acc'' nxt
                  else return acc''
              Assign lhs ex next -> do
                foldJS tr f acc lhs >>= flip (foldJS tr f) ex >>= flip go next
              Forever stm        -> foldJS tr f acc stm
              Return ex          -> foldJS tr f acc ex
              Cont               -> return acc
              Stop               -> return acc
              Tailcall ex        -> foldJS tr f acc ex
              ThunkRet ex        -> foldJS tr f acc ex
        | otherwise =
          f acc stmast
        where !stmast = Stm ast False

instance JSTrav (Exp, Stm) where
  foldMapJS tr fe fs acc (ex, stm) = do
    (acc', stm') <- if tr acc (Stm stm True)
                      then foldMapJS tr fe fs acc stm
                      else return (acc, stm)
    (acc'', ex') <- if tr acc (Exp ex True)
                      then foldMapJS tr fe fs acc' ex
                      else return (acc', ex)
    return (acc'', (ex', stm'))
  foldJS tr f acc (ex, stm) = do
    acc' <- if tr acc (Stm stm True)
              then foldJS tr f acc stm
              else return acc
    if tr acc (Exp ex True)
      then foldJS tr f acc' ex
      else return acc'

instance JSTrav LHS where
  foldMapJS _ _ _ acc lhs@(NewVar _ _) =
    return (acc, lhs)
  foldMapJS t fe fs a (LhsExp r ex) =
    fmap (LhsExp r) <$> foldMapJS t fe fs a ex
  foldJS _ _ acc (NewVar _ _)    = return acc
  foldJS tr f acc (LhsExp _ ex)  = foldJS tr f acc ex

-- | Returns the final statement of a line of statements.
finalStm :: Stm -> TravM Stm
finalStm = go
  where
    go (Case _ _ _ next) = go next
    go (Forever s)       = go s
    go (Assign _ _ next) = go next
    go s                 = return s

-- | Replace the final statement of the given AST with a new one, but only
--   if matches the given predicate.
replaceFinalStm :: Stm -> (Stm -> Bool) -> Stm -> TravM Stm
replaceFinalStm new p = go
  where
    go (Case c d as next) = Case c d as <$> go next
    go (Forever s)        = Forever <$> go s
    go (Assign l r next)  = Assign l r <$> go next
    go s                  = return $ if p s then new else s

-- | Returns statement's returned expression, if any.
finalExp :: Stm -> TravM (Maybe Exp)
finalExp stm = do
  end <- finalStm stm
  case end of
    Return ex -> return $ Just ex
    _         -> return Nothing

class Pred a where
  (.|.) :: a -> a -> a
  (.&.) :: a -> a -> a

instance Pred (a -> b -> Bool) where
  {-# INLINE (.|.) #-}
  {-# INLINE (.&.) #-}
  p .|. q = \a b -> p a b || q a b
  p .&. q = \a b -> p a b && q a b

instance Pred (a -> Bool) where
  {-# INLINE (.|.) #-}
  {-# INLINE (.&.) #-}
  p .|. q = \a -> p a || q a
  p .&. q = \a -> p a && q a

-- | Thunks and explicit lambdas count as lambda abstractions.
{-# INLINE isLambda #-}
isLambda :: ASTNode -> Bool
isLambda = isThunk .|. isFun

{-# INLINE isThunk #-}
isThunk :: ASTNode -> Bool
isThunk (Exp (Thunk _ _) _) = True
isThunk _                   = False

{-# INLINE isFun #-}
isFun :: ASTNode -> Bool
isFun (Exp (Fun _ _) _)   = True
isFun _                   = False

{-# INLINE isLoop #-}
isLoop :: ASTNode -> Bool
isLoop (Stm (Forever _) _) = True
isLoop _                   = False

{-# INLINE isConditional #-}
isConditional :: ASTNode -> Bool
isConditional (Exp _ cond) = cond
isConditional (Stm _ cond) = cond
isConditional _            = False

{-# INLINE isShared #-}
isShared :: ASTNode -> Bool
isShared (Shared _) = True
isShared _          = False

{-# INLINE isSafeForInlining #-}
isSafeForInlining :: ASTNode -> Bool
isSafeForInlining = not <$> isFun .|. isLoop .|. isShared

-- | Counts occurrences. Use ints or something for a more exact count.
data Occs = Never | Once | Lots deriving (Eq, Show)

instance Ord Occs where
  {-# INLINE compare #-}
  compare Never Once = Prelude.LT
  compare Never Lots = Prelude.LT
  compare Once  Lots = Prelude.LT
  compare a b        = if a == b then Prelude.EQ else Prelude.GT

instance Num Occs where
  fromInteger n | n <= 0    = Never
                | n == 1    = Once
                | otherwise = Lots
  Never + x = x
  x + Never = x
  _ + _     = Lots

  Never * _ = Never
  _ * Never = Never
  Once * x  = x
  x * Once  = x
  _ * _     = Lots

  Never - _ = Never
  x - Never = x
  Once - _  = Never
  Lots - _  = Lots

  abs = id

  signum Never = Never
  signum _     = Once

-- | Replace all occurrences of an expression, without entering shared code
--   paths. IO ordering is preserved even when entering lambdas thanks to
--   State# RealWorld.
replaceEx :: JSTrav ast => (ASTNode -> Bool) -> Exp -> Exp -> ast -> TravM ast
replaceEx trav old new =
  mapJS trav (\x -> if x == old then pure new else pure x) pure

-- | Replace all occurrences of an expression, without entering shared code
--   paths. IO ordering is preserved even when entering lambdas thanks to
--   State# RealWorld.
replaceExWithCount :: JSTrav ast
                   => (ASTNode -> Bool) -- ^ Which nodes to enter?
                   -> Exp               -- ^ Expression to replace.
                   -> Exp               -- ^ Replacement expression.
                   -> ast               -- ^ AST to perform replacement on.
                   -> TravM (Int, ast)  -- ^ New AST + count of replacements.
replaceExWithCount trav old new ast =
    foldMapJS (const trav) rep (\count x -> return (count, x)) 0 ast
  where
    rep count ex
      | ex == old = return (count+1, new)
      | otherwise = return (count, ex)
