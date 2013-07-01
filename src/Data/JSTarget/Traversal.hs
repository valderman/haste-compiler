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

class JSTrav ast where
  -- | Bottom up transform over an AST.
  mapJS :: (ASTNode -> Bool)  -- ^ Should the given node be entered or not?
                              --   The appropriate mapping function is always
                              --   applied, however.
        -> (Exp -> TravM Exp) -- ^ Exp to Exp mapping.
        -> (Stm -> TravM Stm) -- ^ Stm to Stm mapping.
        -> ast                -- ^ AST to map over.
        -> TravM ast

  -- | Bottom up fold of an AST.
  foldJS :: (a -> ASTNode -> Bool)    -- ^ Should the given node be entered?
                                      --   The step function is always applied
                                      --   to the current node, however.
         -> (a -> ASTNode -> TravM a) -- ^ Step function.
         -> a                         -- ^ Initial value.
         -> ast                       -- ^ AST to fold over.
         -> TravM a

instance JSTrav a => JSTrav [a] where
  mapJS tr fe fs ast = mapM (mapJS tr fe fs) ast
  foldJS tr f acc ast = foldM (foldJS tr f) acc ast

instance JSTrav Exp where
  mapJS tr fe fs ast = do
      x <- if tr (Exp ast)
             then do
               case ast of
                 v@(Var _)      -> pure v
                 l@(Lit _)      -> pure l
                 Not ex         -> Not <$> mapEx ex
                 BinOp op a b   -> BinOp op <$> mapEx a <*> mapEx b
                 Fun nam vs stm -> Fun nam vs <$> mapJS tr fe fs stm
                 Call ar c f xs -> Call ar c <$> mapEx f <*> mapJS tr fe fs xs
                 Index arr ix   -> Index <$> mapEx arr <*> mapEx ix
                 Arr exs        -> Arr <$> mapM mapEx exs
                 AssignEx l r   -> AssignEx <$> mapEx l <*> mapEx r
                 IfEx c th el   -> IfEx <$> mapEx c <*> mapEx th <*> mapEx el
             else do
               return ast
      fe x
    where
      mapEx = mapJS tr fe fs
  
  foldJS tr f acc ast = do
    let expast = Exp ast
    acc' <- if tr acc expast
              then do
                case ast of
                  Var _         -> do
                    return acc
                  Lit _         -> do
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
  mapJS tr fe fs ast = do
      x <- if tr (Stm ast)
             then do
               case ast of
                 Case ex def alts next ->
                   Case <$> mapJS tr fe fs ex
                        <*> mapJS tr fe fs def
                        <*> mapJS tr fe fs alts
                        <*> mapJS tr fe fs next
                 Forever stm ->
                   Forever <$> mapJS tr fe fs stm
                 Assign lhs ex next ->
                   Assign <$> mapJS tr fe fs lhs
                          <*> mapJS tr fe fs ex
                          <*> mapJS tr fe fs next
                 Return ex ->
                   Return <$> mapJS tr fe fs ex
                 Cont ->
                   return Cont
                 Jump stm ->
                   Jump <$> mapJS tr fe fs stm
                 NullRet ->
                   return NullRet
             else do
               return ast
      fs x

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
  mapJS tr fe fs (ex, stm) = do
    stm' <- mapJS tr fe fs stm
    ex' <- mapJS tr fe fs ex
    return (ex', stm')
  foldJS tr f acc (ex, stm) = do
    acc' <- foldJS tr f acc stm
    foldJS tr f acc' ex

instance JSTrav LHS where
  mapJS _ _ _ lhs@(NewVar _)  = return lhs
  mapJS tr fe fs (LhsExp ex)  = LhsExp <$> mapJS tr fe fs ex
  foldJS _ _ acc (NewVar _)   = return acc
  foldJS tr f acc (LhsExp ex) = foldJS tr f acc ex

instance JSTrav a => JSTrav (Shared a) where
  mapJS tr fe fs sh@(Shared lbl) = do
    when (tr (Label lbl)) $ do
      getRef lbl >>= mapJS tr fe fs >>= putRef lbl
    return sh
  foldJS tr f acc (Shared lbl) = do
    if (tr acc (Label lbl))
      then getRef lbl >>= foldJS tr f acc >>= \acc' -> f acc' (Label lbl)
      else f acc (Label lbl)

instance (JSTrav a, JSTrav b) => JSTrav (Either a b) where
  mapJS tr fe fs (Left x)   = Left <$> mapJS tr fe fs x
  mapJS tr fe fs (Right x)  = Right <$> mapJS tr fe fs x
  foldJS tr f acc (Left x)  = foldJS tr f acc x
  foldJS tr f acc (Right x) = foldJS tr f acc x

class Pred a where
  (.|.) :: a -> a -> a
  (.&.) :: a -> a -> a

instance Pred (a -> Bool) where
  p .|. q = \x -> p x || q x
  p .&. q = \x -> p x && q x

instance Pred (a -> b -> Bool) where
  p .|. q = \a b -> p a b || q a b
  p .&. q = \a b -> p a b && q a b

isShared :: ASTNode -> Bool
isShared (Label _) = True
isShared _         = False

isLambda :: ASTNode -> Bool
isLambda (Exp (Fun _ _ _)) = True
isLambda _               = False

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
