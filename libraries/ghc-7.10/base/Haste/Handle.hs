{-# LANGUAGE ForeignFunctionInterface,
             NoImplicitPrelude,
             MagicHash #-}
module Haste.Handle where
import GHC.Prim
import GHC.HastePrim
import GHC.IO.Handle.Types
import GHC.Base
import GHC.IO (unsafePerformIO)
import GHC.Ptr (Ptr)

type JSHandle = Int

foreign import ccall jsMkStdin :: IO JSHandle
foreign import ccall jsMkStdout :: IO JSHandle
foreign import ccall jsMkStderr :: IO JSHandle
foreign import ccall jsReadHandle :: JSHandle -> Int -> IO (Ptr Any)
foreign import ccall jsWriteHandle :: JSHandle -> Ptr Any -> IO ()
foreign import ccall jsFlushHandle :: JSHandle -> IO ()

jshRead :: Handle -> Int -> IO String
jshRead h len = do
  str <- jsReadHandle (unsafeCoerce# h) len
  return (fromJSStr (unsafeCoerce# str))

jshWrite :: Handle -> String -> IO ()
jshWrite h s = jsWriteHandle (unsafeCoerce# h) (unsafeCoerce# (toJSStr s))

jshFlush :: Handle -> IO ()
jshFlush h = jsFlushHandle (unsafeCoerce# h)

stdout, stderr, stdin :: Handle
{-# NOINLINE stdout #-}
stdout = unsafePerformIO (jsMkStdout >>= \h -> unsafeCoerce# h)
{-# NOINLINE stdin #-}
stdin = unsafePerformIO (jsMkStdin >>= \h -> unsafeCoerce# h)
{-# NOINLINE stderr #-}
stderr = unsafePerformIO (jsMkStderr >>= \h -> unsafeCoerce# h)

data FakePtr a = FakePtr a

toPtr :: a -> Ptr a
toPtr x = unsafeCoerce# (FakePtr x)

fromPtr :: Ptr a -> a
fromPtr ptr =
  case unsafeCoerce# ptr of
    FakePtr val -> val
