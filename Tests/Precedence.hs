module Tests.Precedence where

a,b,c,d,e :: IO Int
{-# NOINLINE a #-}
{-# NOINLINE b #-}
{-# NOINLINE c #-}
{-# NOINLINE d #-}
{-# NOINLINE e #-}
a = return 5
b = return 19
c = return 47
d = return 99
e = return 41

runTest :: IO [Int]
runTest = do
  [x,y,z,u,v] <- sequence [a,b,c,d,e]
  return [x-y-z, x-(y-z), x+y+u-v+z, x+(y+u)-(v+z), x*y`div`z+v-u]
