{-# LANGUAGE CPP, OverloadedStrings #-}
module Tests.FFI where
import Haste.Foreign

add :: Int -> Double -> IO Double
rev :: String -> IO String
#ifdef __HASTE__
add = ffi "(function(a, b) {return a+b;})"
rev = ffi "(function(s) {return Haste.reverse(s);})"
#else
add a b = return $ fromIntegral a + b
rev = return . reverse
#endif

r :: String -> IO String
r = return . reverse

runTest :: IO (Double, String)
runTest = do
#ifdef __HASTE__
  export "reverse" r
#endif
  x <- add 5 2.2
  s <- rev "lol internet"
  return (x, s)
