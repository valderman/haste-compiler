module Tests.Take where

runTest :: IO String
runTest =
    return $ if 5 == (length $ take 5 $ [1,2,3,4,5,6,7,8,9]) then ":)" else ":("
