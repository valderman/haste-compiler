{-# LANGUAGE DeriveGeneric, OverloadedStrings, CPP #-}
module Tests.GenericToFromAny where
import Haste.Foreign
import GHC.Generics

data Foo a = Foo {
    foo :: Maybe (Foo a),
    bar :: [Bar],
    baz :: Baz a Bool,
    buu :: Bool
  } deriving (Generic, Eq)

instance ToAny a => ToAny (Foo a)
instance ToAny Bar
instance (ToAny a, ToAny b) => ToAny (Baz a b)

instance (ToAny a, FromAny a) => FromAny (Foo a)
instance FromAny Bar
instance (ToAny a, ToAny b, FromAny a, FromAny b) => FromAny (Baz a b)

data Bar = A | B
  deriving (Generic, Eq)
data Baz a b = Baz Int a b
  deriving (Generic, Eq)

{-# NOINLINE test #-}
test :: IO (Foo String)
test = return $ Foo {
    foo = Just (Foo (Just $ Foo Nothing [B] (Baz 42 "" False) True) [] (Baz 0 "z" True) False),
    bar = [A,B,B,A],
    baz = Baz 1 "some string" False,
    buu = True
  }

runTest :: IO Bool
runTest = do
#ifdef __HASTE__
  x <- test
  y <- test >>= fromAny . toAny
  return $ x == y
#else
  return True
#endif
