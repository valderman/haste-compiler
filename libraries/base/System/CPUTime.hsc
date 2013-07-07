{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NondecreasingIndentation, ForeignFunctionInterface, CApiFFI #-}

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

getCPUTime :: IO Integer
getCPUTime = return 0

cpuTimePrecision :: Integer
cpuTimePrecision = 0