{-# LANGUAGE CPP, OverloadedStrings #-}
module Tests.JSString_JSON where

#ifdef __HASTE__
import Data.List
import Haste.JSON
import Haste.Prim

runTest = putStrLn $ toString (Dict [("foo", Str "bar")])

toString :: JSON -> String
toString (Dict fields) =
    intercalate "" (fmap (\ (key, value) ->
        fromJSStr key ++ " " ++ toString value) fields)
toString (Str s) = fromJSStr s
#else
runTest = putStrLn "foo bar"
#endif
