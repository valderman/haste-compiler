module Tests.UnboxedTupleUsingScrutinee where

loopWhile :: (Bool -> Bool) -> IO Bool -> IO Bool
loopWhile f a = a >>= \r -> if f r then return r else loopWhile f a

runTest :: IO ()
runTest = loopWhile (const True) (return True) >>= putStrLn . show
