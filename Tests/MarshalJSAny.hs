{-# LANGUAGE CPP, OverloadedStrings #-}
module Tests.MarshalJSAny where
import Haste
import Haste.Foreign
import Haste.JSON

extract :: JSAny -> IO (Int, String)
test :: JSAny
#ifdef __HASTE__
extract = ffi "(function(x){return [x.foo, x.bar];})"
test = toObject $ Dict [("foo", 42), ("bar", "test")]
#else
extract _ = return $ (42, "test")
test = undefined
#endif

runTest :: IO (Int, String)
runTest = extract test
