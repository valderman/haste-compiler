{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
-- | QuickCheck stuff for the AST parts.
module Data.JSTarget.QuickCheck where
import Prelude hiding (LT, GT)
import Control.Applicative
import Test.QuickCheck
import Data.JSTarget.AST
import Data.JSTarget.Op
import Data.JSTarget.Constructors
import Data.JSTarget.Traversal
import qualified Data.Map as M

instance Arbitrary Name where
  arbitrary = Name <$> arbitrary <*> arbitrary

instance Arbitrary Var where
  arbitrary = oneof [Foreign <$> arbitrary,
                     Internal <$> arbitrary <*> arbitrary]

instance Arbitrary LHS where
  arbitrary = oneof [NewVar <$> arbitrary <*> arbitrary, LhsExp <$> arbitrary]

instance Arbitrary Call where
  arbitrary = oneof [pure Normal, pure Fast, Method <$> arbitrary]

instance Arbitrary Lit where
  arbitrary = oneof [LNum <$> arbitrary,
                     LStr <$> arbitrary,
                     LBool <$> arbitrary,
                     LInt <$> arbitrary]

instance Arbitrary BinOp where
  arbitrary = oneof $ map pure $ [Add,Mul,Sub,Div,Mod,And,Or,Eq,Neq,LT,GT,LTE,
                                  GTE,Shl,ShrL,ShrA,BitAnd,BitOr,BitXor]

instance Arbitrary Exp where
  arbitrary = sized $ \n -> resize (n `div` 2) $ do
    frequency [
        (1, Var <$> arbitrary),
        (1, Lit <$> arbitrary),
        (n, Not <$> arbitrary),
        (n, BinOp <$> arbitrary <*> arbitrary <*> arbitrary),
        (n, Fun <$> arbitrary <*> arbitrary <*> arbitrary),
        (n, Call <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary),
        (n, Index <$> arbitrary <*> arbitrary),
        (n, Arr <$> arbitrary),
        (n, AssignEx <$> arbitrary <*> arbitrary),
        (n, IfEx <$> arbitrary <*> arbitrary <*> arbitrary)
      ]

instance Arbitrary Stm where
  arbitrary = sized $ \n -> resize (n `div` 2) $ do
    frequency [
        (n, genCase),
        (n, Forever <$> arbitrary),
        (n, Assign <$> arbitrary <*> arbitrary <*> arbitrary),
        (n, Return <$> arbitrary),
        (1, pure Cont),
        (1, pure NullRet)
      ]

instance Arbitrary (Stm -> Stm) where
  arbitrary = sized $ \n -> resize (n `div` 2) $ do
    frequency [
      (n, do
          next <- arbitrary
          return (Forever . next)),
      (n, do
          next <- arbitrary
          lhs <- arbitrary
          rhs <- arbitrary
          return (Assign lhs rhs . next)),
      (1, do
          c <- genCase
          return (const c)),
      (1, do
          return id)
      ]

genCase = do
  scrut <- arbitrary
  def <- arbitrary
  ref <- Lbl <$> arbitrary <*> arbitrary
  alts <- map (\f -> f (Jump (Shared ref))) <$> arbitrary
  lits <- map Lit <$> arbitrary
  return $ Case scrut def (zip lits alts) (Shared ref)

instance Arbitrary a => Arbitrary (AST a) where
  arbitrary = AST <$> arbitrary <*> pure M.empty

-- | Does mapJS preserve not mangle its argument?
prop_mapId :: AST Exp -> Bool
prop_mapId ast@(AST c j) =
  -- No shared, since we don't generate jump tables.
  runTravM (mapJS (not <$> isShared) pure pure c) j == ast
