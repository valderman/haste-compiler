{-# LANGUAGE FlexibleInstances, TupleSections, PatternGuards #-}
-- | Generic traversal of JSTarget AST types.
module Data.JSTarget.Traversal where
import Control.Applicative
import Control.Monad
import Data.JSTarget.AST
import Data.Map as M ((!), insert)

-- | AST nodes we'd like to fold and map over.
data ASTNode = Exp Exp | Stm Stm | Label Lbl

newtype TravM a = T (JumpTable -> (JumpTable, a))
instance Monad TravM where
  return x  = T $ \js -> (js, x)
  T m >>= f = T $ \js ->
    case m js of
      (js', x) | T f' <- f x -> f' js'

instance Applicative TravM where
  pure  = return
  (<*>) = ap

instance Functor TravM where
  fmap f (T m) = T $ \js -> fmap f (m js)

runTravM :: TravM a -> JumpTable -> AST a
runTravM (T f) js = case f js of (js', x) -> AST x js'

getRef :: Lbl -> TravM Stm
getRef lbl = T $ \js -> (js, js M.! lbl)

putRef :: Lbl -> Stm -> TravM ()
putRef lbl stm = T $ \js -> (M.insert lbl stm js, ())

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
  foldMapJS tr fe fs acc ast = do
      (acc', x) <- if tr acc (Exp ast)
                     then do
                       case ast of
                         v@(Var _)      -> do
                           pure (acc, v)
                         l@(Lit _)      -> do
                           pure (acc, l)
                         v@(Verbatim _) -> do
                           pure (acc, v)
                         Not ex         -> do
                           fmap Not <$> mapEx acc ex
                         BinOp op a b   -> do
                           (acc', a') <- mapEx acc a
                           (acc'', b') <- mapEx acc' b
                           return (acc'', BinOp op a' b')
                         Fun nam vs stm -> do
                           fmap (Fun nam vs) <$> foldMapJS tr fe fs acc stm
                         Call ar c f xs -> do
                           (acc', f') <- mapEx acc f
                           (acc'', xs') <- foldMapJS tr fe fs acc' xs
                           return (acc'', Call ar c f' xs')
                         Index arr ix   -> do
                           (acc', arr') <- mapEx acc arr
                           (acc'', ix') <- mapEx acc' ix
                           return (acc'', Index arr' ix')
                         Arr exs        -> do
                           fmap Arr <$> foldMapJS tr fe fs acc exs
                         AssignEx l r   -> do
                           (acc', l') <- mapEx acc l
                           (acc'', r') <- mapEx acc' r
                           return (acc'', AssignEx l' r')
                         IfEx c th el   -> do
                           (acc', c') <- mapEx acc c
                           (acc'', th') <- mapEx acc' th
                           (acc''', el') <- mapEx acc'' el
                           return (acc''', IfEx c' th' el')
                     else do
                       return (acc, ast)
      fe acc' x
    where
      mapEx = foldMapJS tr fe fs
  
  foldJS tr f acc ast = do
    let expast = Exp ast
    acc' <- if tr acc expast
              then do
                case ast of
                  Var _         -> do
                    return acc
                  Lit _         -> do
                    return acc
                  Verbatim _    -> do
                    return acc
                  Not ex        -> do
                    foldJS tr f acc ex
                  BinOp _ a b  -> do
                    acc' <- foldJS tr f acc a
                    foldJS tr f acc' b
                  Fun _ _ stm    -> do
                    foldJS tr f acc stm
                  Call _ _ fun xs -> do
                    acc' <- foldJS tr f acc fun
                    foldJS tr f acc' xs
                  Index arr ix  -> do
                    acc' <- foldJS tr f acc arr
                    foldJS tr f acc' ix
                  Arr exs       -> do
                    foldJS tr f acc exs
                  AssignEx l r  -> do
                    acc' <- foldJS tr f acc l
                    foldJS tr f acc' r
                  IfEx c th el  -> do
                    acc' <- foldJS tr f acc c
                    acc'' <- foldJS tr f acc' th
                    foldJS tr f acc'' el
              else do
                return acc
    f acc' expast

instance JSTrav Stm where
  foldMapJS tr fe fs acc ast = do
      (acc', x) <- if tr acc (Stm ast)
                     then do
                       case ast of
                         Case ex def alts next -> do
                           (acc', ex') <- foldMapJS tr fe fs acc ex
                           (acc'', def') <- foldMapJS tr fe fs acc' def
                           (acc''', alts') <- foldMapJS tr fe fs acc'' alts
                           (acc'''', next') <- foldMapJS tr fe fs acc''' next
                           return (acc'''', Case ex' def' alts' next')
                         Forever stm -> do
                           fmap Forever <$> foldMapJS tr fe fs acc stm
                         Assign lhs ex next -> do
                           (acc', lhs') <- foldMapJS tr fe fs acc lhs
                           (acc'', ex') <- foldMapJS tr fe fs acc' ex
                           (acc''', next') <- foldMapJS tr fe fs acc'' next
                           return (acc''', Assign lhs' ex' next')
                         Return ex -> do
                           fmap Return <$> foldMapJS tr fe fs acc ex
                         Cont -> do
                           return (acc, Cont)
                         Jump stm -> do
                           fmap Jump <$> foldMapJS tr fe fs acc stm
                         NullRet -> do
                           return (acc, NullRet)
                     else do
                       return (acc, ast)
      fs acc' x

  foldJS tr f acc ast = do
    let stmast = Stm ast
    acc' <- if tr acc stmast
              then do
                case ast of
                  Case ex def alts next -> do
                    acc' <- foldJS tr f acc ex
                    acc'' <- foldJS tr f acc' def
                    acc''' <- foldJS tr f acc'' alts
                    foldJS tr f acc''' next
                  Forever stm -> do
                    foldJS tr f acc stm
                  Assign lhs ex next -> do
                    acc' <- foldJS tr f acc lhs
                    acc'' <- foldJS tr f acc' ex
                    foldJS tr f acc'' next
                  Return ex -> do
                    foldJS tr f acc ex
                  Cont -> do
                    return acc
                  Jump j -> do
                    foldJS tr f acc j
                  NullRet -> do
                    return acc
              else do
                return acc
    f acc' stmast

instance JSTrav (Exp, Stm) where
  foldMapJS tr fe fs acc (ex, stm) = do
    (acc', stm') <- foldMapJS tr fe fs acc stm
    (acc'', ex') <- foldMapJS tr fe fs acc' ex
    return (acc'', (ex', stm'))
  foldJS tr f acc (ex, stm) = do
    acc' <- foldJS tr f acc stm
    foldJS tr f acc' ex

instance JSTrav LHS where
  foldMapJS _ _ _ acc lhs@(NewVar _ _) = return (acc, lhs)
  foldMapJS t fe fs a (LhsExp ex)      = fmap LhsExp <$> foldMapJS t fe fs a ex
  foldJS _ _ acc (NewVar _ _)  = return acc
  foldJS tr f acc (LhsExp ex)  = foldJS tr f acc ex

instance JSTrav a => JSTrav (Shared a) where
  foldMapJS tr fe fs acc sh@(Shared lbl) = do
    if (tr acc (Label lbl)) 
      then do
        stm <- getRef lbl
        (acc', stm') <- foldMapJS tr fe fs acc stm
        putRef lbl stm'
        return (acc', sh)
      else do
        return (acc, sh)
  foldJS tr f acc (Shared lbl) = do
    if (tr acc (Label lbl))
      then getRef lbl >>= foldJS tr f acc >>= \acc' -> f acc' (Label lbl)
      else f acc (Label lbl)

-- | Returns the final statement of a line of statements.
finalStm :: Stm -> TravM Stm
finalStm = go
  where
    go (Case _ _ _ (Shared next)) = getRef next >>= go
    go (Forever s)                = go s
    go (Assign _ _ next)          = go next
    go (Jump (Shared next))       = getRef next >>= go
    go s@(Return _)               = return s
    go (Cont)                     = return Cont
    go (NullRet)                  = return NullRet

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
  p .|. q = \a b -> p a b || q a b
  p .&. q = \a b -> p a b && q a b

instance Pred (a -> Bool) where
  p .|. q = \a -> p a || q a
  p .&. q = \a -> p a && q a

isShared :: ASTNode -> Bool
isShared (Label _) = True
isShared _         = False

isLambda :: ASTNode -> Bool
isLambda (Exp (Fun _ _ _)) = True
isLambda _                 = False

isJump :: ASTNode -> Bool
isJump (Stm (Jump _)) = True
isJump _              = False

-- | Counts occurrences. Use ints or something for a more exact count.
data Occs = Never | Once | Lots deriving (Eq, Show)

instance Ord Occs where
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

  abs = id

  signum Never = Never
  signum _     = Once
