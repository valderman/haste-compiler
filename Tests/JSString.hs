{-# LANGUAGE OverloadedStrings #-}
module Tests.JSString where
import Haste.JSString (JSString)
import qualified Haste.JSString as J
import Data.Char

{-# NOINLINE s1 #-}
s1 :: IO JSString
s1 = return "string number one"

{-# NOINLINE s2 #-}
s2 :: IO JSString
s2 = return "二つ目のストリング"

out :: JSString -> IO ()
out = putStrLn . J.unpack

runTest = do
  s <- s1
  s' <- s2
  out $ J.reverse $ J.map (chr . (+1) . ord) s
  out $ s `J.append` s'
  let str = reverse $ J.unpack s'
  print $ J.foldl' (\a x -> a + ord x) 0 $ J.pack str
  print $ J.foldr (\x a -> a + ord x) 0 $ J.concat [s,s',"hi"]
  print $ J.length s'
  out $ J.tail $ J.replicate 10 'x'
  J.putStrLn $ 'x' `J.cons` s
  J.putStrLn $ s `J.snoc` 'ö'
  J.putStrLn $ J.concatMap (J.cons 'x' . J.singleton) "abc"
  print $ J.any (== 'g') s
  print $ J.any (== 'g') s'
  print $ J.all (> 'g') s
  let (a, b) = J.splitAt 5 s'
  print $ J.all (> 'g') a
  print (b, a)
  print $ s' J.! 2
  out $ J.init s
  out $ J.tail s
  out $ J.drop 3 s
  out $ J.take 3 s

