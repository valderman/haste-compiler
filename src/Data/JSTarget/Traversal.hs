{-# LANGUAGE FlexibleInstances, TupleSections #-}
-- | Generic traversal of JSTarget AST types.
module Data.JSTarget.Traversal where
import Control.Applicative
import Control.Monad
import Data.JSTarget.AST

-- | AST nodes we'd like to fold and map over.
data ASTNode = Exp Exp | Stm Stm | Label Lbl

class JSTrav ast where
  -- | Bottom up transform over an AST.
  mapJS :: (ASTNode -> Bool) -- ^ Should the given node be entered or not?
                             --   The appropriate mapping function is always
                             --   applied, however.
        -> (Exp -> AST Exp)  -- ^ Exp to Exp mapping.
        -> (Stm -> AST Stm)  -- ^ Stm to Stm mapping.
        -> ast               -- ^ AST to map over.
        -> AST ast

  -- | Bottom up fold of an AST.
  foldJS :: (a -> ASTNode -> Bool)  -- ^ Should the given node be entered?
                                    --   The step function is always applied to
                                    --   the current node, however.
         -> (a -> ASTNode -> AST a) -- ^ Step function.
         -> a                       -- ^ Initial value.
         -> ast                     -- ^ AST to fold over.
         -> AST a

instance JSTrav a => JSTrav [a] where
  mapJS tr fe fs ast = mapM (mapJS tr fe fs) ast
  foldJS tr f acc ast = foldM (foldJS tr f) acc ast

instance JSTrav Exp where
  mapJS tr fe fs ast = do
      x <- if tr (Exp ast)
             then do
               case ast of
                 Not ex         -> Not <$> mapEx ex
                 BinOp op a b   -> BinOp op <$> mapEx a <*> mapEx b
                 Fun vs stm     -> Fun vs <$> mapJS tr fe fs stm
                 Call ar c f xs -> Call ar c <$> mapEx f <*> mapJS tr fe fs xs
                 Index arr ix   -> Index <$> mapEx arr <*> mapEx ix
                 Arr exs        -> Arr <$> mapM mapEx exs
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
                  Not ex        -> do
                    foldJS tr f acc ex
                  BinOp op a b  -> do
                    acc' <- foldJS tr f acc a
                    foldJS tr f acc' b
                  Fun vs stm    -> do
                    foldJS tr f acc stm
                  Call _ _ fun xs -> do
                    acc' <- foldJS tr f acc fun
                    foldJS tr f acc' xs
                  Index arr ix  -> do
                    acc' <- foldJS tr f acc arr
                    foldJS tr f acc' ix
                  Arr exs       -> do
                    foldJS tr f acc exs
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
              else do
                return acc
    f acc' stmast

instance JSTrav (Lit, Stm) where
  mapJS tr fe fs (l, stm) = do
    stm' <- mapJS tr fe fs stm
    return (l, stm')
  foldJS tr f acc (l, stm) = do
    foldJS tr f acc stm

instance JSTrav LHS where
  mapJS tr fe fs lhs@(NewVar _) = return lhs
  mapJS tr fe fs (LhsExp ex)    = LhsExp <$> mapJS tr fe fs ex
  foldJS tr f acc lhs@(NewVar _) = return acc
  foldJS tr f acc (LhsExp ex)    = foldJS tr f acc ex

instance JSTrav a => JSTrav (Shared a) where
  mapJS tr fe fs sh@(Shared lbl) = do
    when (tr (Label lbl)) $ do
      getRef lbl >>= mapJS tr fe fs >>= putRef lbl
    return sh
  foldJS tr f acc sh@(Shared lbl) = do
    if (tr acc (Label lbl))
      then getRef lbl >>= foldJS tr f acc >>= \acc' -> f acc' (Label lbl)
      else f acc (Label lbl)

-- | Fold an expression bottom up, without entering lambdas.
foldEx :: (a -> Exp -> a) -> a -> Exp -> a
foldEx f acc ex@(Var _) =
  f acc ex
foldEx f acc ex@(Lit _) =
  f acc ex
foldEx f acc ex@(Not ex') =
  f (foldEx f acc ex') ex
foldEx f acc ex@(BinOp op a b) =
  f (foldEx f (foldEx f acc b) a) ex
foldEx f acc ex@(Fun _ _) =
  f acc ex
foldEx f acc ex@(Call arity call fun args) =
  f (foldEx f (foldr (flip $ foldEx f) acc args) fun) ex
foldEx f acc ex@(Index arr ix) =
  f (foldEx f (foldEx f acc ix) arr) ex
foldEx f acc (Arr arr) =
  foldr (flip $ foldEx f) acc arr

class Pred a where
  (.|.) :: a -> a -> a

instance Pred (a -> Bool) where
  p .|. q = \x -> p x || q x

instance Pred (a -> b -> Bool) where
  p .|. q = \a b -> p a b || q a b

isShared :: ASTNode -> Bool
isShared (Label _) = True
isShared _         = False

isLambda :: ASTNode -> Bool
isLambda (Exp (Fun _ _)) = True
isLambda _               = False

-- | Counts occurrences. Use ints or something for a more exact count.
data Occs = Never | Once | Lots deriving Eq

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

  Never * x = 0
  x * Never = 0
  Once * x  = x
  x * Once  = x
  _ * _     = Lots

  abs = id

  signum Never = Never
  signum _     = Once
