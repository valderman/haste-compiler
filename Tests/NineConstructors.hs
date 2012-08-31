module Tests.NineConstructors where

data T = T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 deriving Eq

runTest :: IO Bool
runTest = return $ T1 == T2
