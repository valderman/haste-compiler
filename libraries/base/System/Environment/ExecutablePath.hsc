{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Environment.ExecutablePath
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Function to retrieve the absolute filepath of the current executable.
--
-----------------------------------------------------------------------------

module System.Environment.ExecutablePath ( getExecutablePath ) where

-- The imports are purposely kept completely disjoint to prevent edits
-- to one OS implementation from breaking another.

#if defined(darwin_HOST_OS)
import Data.Word
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
#elif defined(linux_HOST_OS)
import Foreign.C
import Foreign.Marshal.Array
#elif defined(mingw32_HOST_OS)
import Data.Word
import Foreign.C
import Foreign.Marshal.Array
import Foreign.Ptr
#else
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
#endif

-- The exported function is defined outside any if-guard to make sure
-- every OS implements it with the same type.

-- | Returns the absolute pathname of the current executable.
--
-- Note that for scripts and interactive sessions, this is the path to
-- the interpreter (e.g. ghci.)
getExecutablePath :: IO FilePath

getExecutablePath = error "There is no executable."

--------------------------------------------------------------------------------
