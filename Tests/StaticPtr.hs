{-# LANGUAGE StaticPointers #-}
module Tests.StaticPtr where
import GHC.StaticPtr

{-# NOINLINE ptr #-}
ptr :: StaticPtr String
ptr = static "This is a "

{-# NOINLINE ptr2 #-}
ptr2 :: StaticPtr String
ptr2 = static "string which is"

{-# NOINLINE ptr3 #-}
ptr3 :: StaticPtr String
ptr3 = static " statically pointed to."

getptr :: StaticPtr a -> IO (Maybe a)
getptr = fmap (fmap deRefStaticPtr) . unsafeLookupStaticPtr . staticKey

runTest :: IO ()
runTest = do
  print (staticKey (static staticPtrKeys))
  print (staticKey (static runTest))
  [Just x, Just z, Just y] <- mapM getptr [ptr, ptr3, ptr2]
  putStrLn $ concat [x, y, z]
