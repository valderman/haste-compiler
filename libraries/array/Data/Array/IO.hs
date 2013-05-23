{-# OPTIONS_GHC -#include "HsBase.h" #-}
{-# OPTIONS_GHC -w #-} --tmp
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.IO
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Array.MArray)
--
-- Mutable boxed and unboxed arrays in the IO monad.
--
-----------------------------------------------------------------------------

module Data.Array.IO (
    -- * @IO@ arrays with boxed elements
    IOArray,             -- instance of: Eq, Typeable

    -- * @IO@ arrays with unboxed elements
    IOUArray,            -- instance of: Eq, Typeable
    castIOUArray,        -- :: IOUArray i a -> IO (IOUArray i b)

    -- * Overloaded mutable array interface
    module Data.Array.MArray,

    -- * Doing I\/O with @IOUArray@s
    hGetArray,           -- :: Handle -> IOUArray Int Word8 -> Int -> IO Int
    hPutArray,           -- :: Handle -> IOUArray Int Word8 -> Int -> IO ()
  ) where

import Data.Array.Base
import Data.Array.IO.Internals hiding ( castIOUArray )
import qualified Data.Array.Unsafe as U ( castIOUArray )
import Data.Array.MArray
import System.IO.Error

#ifdef __GLASGOW_HASKELL__
import Foreign
import Foreign.C

import GHC.Exts  (MutableByteArray#, RealWorld)
import GHC.Arr
import GHC.IORef
import GHC.IO.Handle
import GHC.IO.Buffer
import GHC.IO.Exception

#else
import Data.Char
import Data.Word ( Word8 )
import System.IO
#endif

#ifdef __GLASGOW_HASKELL__
-- ---------------------------------------------------------------------------
-- hGetArray

-- | Reads a number of 'Word8's from the specified 'Handle' directly
-- into an array.
hGetArray
        :: Handle               -- ^ Handle to read from
        -> IOUArray Int Word8   -- ^ Array in which to place the values
        -> Int                  -- ^ Number of 'Word8's to read
        -> IO Int
                -- ^ Returns: the number of 'Word8's actually
                -- read, which might be smaller than the number requested
                -- if the end of file was reached.

hGetArray handle (IOUArray (STUArray _l _u n ptr)) count
  | count == 0              = return 0
  | count < 0 || count > n  = illegalBufferSize handle "hGetArray" count
  | otherwise = do
      -- we would like to read directly into the buffer, but we can't
      -- be sure that the MutableByteArray# is pinned, so we have to
      -- allocate a separate area of memory and copy.
      allocaBytes count $ \p -> do
        r <- hGetBuf handle p count
        memcpy_ba_ptr ptr p (fromIntegral r)
        return r

foreign import ccall unsafe "memcpy"
   memcpy_ba_ptr :: MutableByteArray# RealWorld -> Ptr a -> CSize -> IO (Ptr ())

-- ---------------------------------------------------------------------------
-- hPutArray

-- | Writes an array of 'Word8' to the specified 'Handle'.
hPutArray
        :: Handle                       -- ^ Handle to write to
        -> IOUArray Int Word8           -- ^ Array to write from
        -> Int                          -- ^ Number of 'Word8's to write
        -> IO ()

hPutArray handle (IOUArray (STUArray _l _u n raw)) count
  | count == 0              = return ()
  | count < 0 || count > n  = illegalBufferSize handle "hPutArray" count
  | otherwise = do
      -- as in hGetArray, we would like to use the array directly, but
      -- we can't be sure that the MutableByteArray# is pinned.
     allocaBytes count $ \p -> do
       memcpy_ptr_ba p raw (fromIntegral count)
       hPutBuf handle p count

foreign import ccall unsafe "memcpy"
   memcpy_ptr_ba :: Ptr a -> MutableByteArray# RealWorld -> CSize -> IO (Ptr ())

-- ---------------------------------------------------------------------------
-- Internal Utils

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz =
        ioException (ioeSetErrorString
                     (mkIOError InvalidArgument fn (Just handle) Nothing)
                     ("illegal buffer size " ++ showsPrec 9 (sz::Int) []))

#else /* !__GLASGOW_HASKELL__ */
hGetArray :: Handle -> IOUArray Int Word8 -> Int -> IO Int
hGetArray handle arr count = do
        bds <- getBounds arr
        if count < 0 || count > rangeSize bds
           then illegalBufferSize handle "hGetArray" count
           else get 0
 where
  get i | i == count = return i
        | otherwise = do
                error_or_c <- try (hGetChar handle)
                case error_or_c of
                    Left ex
                        | isEOFError ex -> return i
                        | otherwise -> ioError ex
                    Right c -> do
                        unsafeWrite arr i (fromIntegral (ord c))
                        get (i+1)

hPutArray :: Handle -> IOUArray Int Word8 -> Int -> IO ()
hPutArray handle arr count = do
        bds <- getBounds arr
        if count < 0 || count > rangeSize bds
           then illegalBufferSize handle "hPutArray" count
           else put 0
 where
  put i | i == count = return ()
        | otherwise = do
                w <- unsafeRead arr i
                hPutChar handle (chr (fromIntegral w))
                put (i+1)

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize _ fn sz = ioError $
        userError (fn ++ ": illegal buffer size " ++ showsPrec 9 (sz::Int) [])
#endif /* !__GLASGOW_HASKELL__ */


{-# DEPRECATED castIOUArray
              "Please import from Data.Array.Unsafe instead; This will be removed in the next release"
 #-}
-- | Casts an 'IOUArray' with one element type into one with a
-- different element type.  All the elements of the resulting array
-- are undefined (unless you know what you\'re doing...).
{-# INLINE castIOUArray #-}
castIOUArray :: IOUArray i a -> IO (IOUArray i b)
castIOUArray = U.castIOUArray

