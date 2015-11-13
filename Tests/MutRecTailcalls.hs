{-# LANGUAGE CPP #-}
-- Test case contributed by Jason Priestley
module Tests.MutRecTailcalls where
import Haste

runTest :: IO ()
#ifdef __HASTE__
runTest = go

go = one "abc"
  where
    one xs = case xs of
      'a':xs' -> writeLog "a1" >> two xs'
      'b':xs' -> writeLog "b1" >> two xs'
      'c':xs' -> writeLog "c1" >> two xs'
      [] -> return ()
    two xs = case xs of
      'a':_ -> let xs' = tail xs in writeLog "a2" >> one xs'
      'b':_ -> let xs' = tail xs in writeLog "b2" >> one xs'
      'c':_ -> let xs' = tail xs in writeLog "c2" >> one xs'
      [] -> return ()
#else
runTest = putStrLn "a1\nb2\nc1"
#endif
