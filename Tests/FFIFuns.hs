{-# LANGUAGE OverloadedStrings, CPP #-}
module Tests.FFIFuns where
import Haste
import Haste.Foreign

#ifdef __HASTE__

test3 :: (Maybe Int -> Maybe String -> Bool -> IO String) -> IO String
test3 = ffi "(function(f) {return (f(null, 'str', true));})"

test1 :: (Int -> IO Int) -> IO ()
test1 = ffi "(function(f) {console.log(f(42));})"

test0 :: IO Int -> IO Int
test0 = ffi "(function(f) {return f()+1;})"

testNothing :: IO () -> IO (Maybe Int, Maybe Int)
testNothing = ffi "(function(f) {return [17, null];})"

runTest = do
  test0 (return 42) >>= putStrLn . show
  res <- test3 (\a b c -> return $ show a ++ show (fmap reverse b) ++ show c)
  putStrLn res
  test1 (\x -> putStrLn ("Got: " ++ show x) >> return 9)
  testNothing (putStrLn "this should never happen") >>= putStrLn . show

#else

runTest = do
  putStrLn $ show (43 :: Int)
  putStrLn $ show (Nothing :: Maybe Int) ++
             show (fmap reverse (Just ("str" :: String))) ++
             show True
  putStrLn $ "Got: 42"
  putStrLn $ show (9 :: Int)
  putStrLn $ show (Just (17 :: Int), Nothing :: Maybe Int)

#endif
