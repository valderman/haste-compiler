module Tests.Unicode where
import Data.Char

{-# NOINLINE str #-}
str :: String
str = "痴漢は犯罪だ！"

runTest :: IO [String]
runTest =
    return [str, str', str'', str''']
  where
    str' = filter (/= 'だ') str
    str'' = map toUpper str'
    str''' = map toLower $ str'' ++ " THIS IS ENGLISH"
