{-# LANGUAGE OverloadedStrings, CPP #-}
module Tests.NewSeed where
import Haste
import Haste.Foreign
import Haste.App 

consoleLog_ffi :: String -> IO ()
#ifdef __HASTE__
consoleLog_ffi = ffi "(function (msg) { console.log(msg); })"
#else
consoleLog_ffi = putStrLn
#endif

foo_ffi :: IO () -> IO ()
#ifdef __HASTE__
foo_ffi = ffi "(function (cb) { cb(); })"
#else
foo_ffi = id
#endif

runTest :: IO ()
runTest = foo_ffi(loadCallback)
  where
    loadCallback = do
        gen <- next `fmap` newSeed
        consoleLog_ffi "hello world2"
#ifdef __HASTE__
        print $ fst $ randomR (0, 1000 :: Int) (next $ next $ mkSeed 0)
#else
        putStrLn "952"
#endif
