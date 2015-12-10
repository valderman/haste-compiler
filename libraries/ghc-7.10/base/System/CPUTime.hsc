{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NondecreasingIndentation, CApiFFI #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.CPUTime
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The standard CPUTime library.
--
-----------------------------------------------------------------------------

#include "HsFFI.h"
#include "HsBaseConfig.h"

module System.CPUTime 
        (
         getCPUTime,       -- :: IO Integer
         cpuTimePrecision  -- :: Integer
        ) where

import Data.Ratio

import Foreign
import Foreign.C

-- For struct rusage
#if !defined(mingw32_HOST_OS) && !defined(irix_HOST_OS)
# if HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
# endif
#endif

-- For FILETIME etc. on Windows
#if HAVE_WINDOWS_H
#include <windows.h>
#endif

-- for struct tms
#if HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif

##ifdef mingw32_HOST_OS
## if defined(i386_HOST_ARCH)
##  define WINDOWS_CCONV stdcall
## elif defined(x86_64_HOST_ARCH)
##  define WINDOWS_CCONV ccall
## else
##  error Unknown mingw32 arch
## endif
##else
##endif

#if !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS)
realToInteger :: Real a => a -> Integer
realToInteger ct = round (realToFrac ct :: Double)
  -- CTime, CClock, CUShort etc are in Real but not Fractional, 
  -- so we must convert to Double before we can round it
#endif

-- -----------------------------------------------------------------------------
-- |Computation 'getCPUTime' returns the number of picoseconds CPU time
-- used by the current program.  The precision of this result is
-- implementation-dependent.

getCPUTime :: IO Integer
getCPUTime = return 0


-- |The 'cpuTimePrecision' constant is the smallest measurable difference
-- in CPU time that the implementation can record, and is given as an
-- integral number of picoseconds.
--
-- On Haste, currently the heat death of universe will happen before the next
-- tick occurs.
cpuTimePrecision :: Integer
cpuTimePrecision = 2^64

foreign import ccall unsafe clk_tck :: CLong

clockTicks :: Int
clockTicks = fromIntegral clk_tck
