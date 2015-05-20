{-# LANGUAGE CPP, MagicHash, UnboxedTuples, NoImplicitPrelude, 
             GHCForeignImportPrim #-}
{-# OPTIONS_HADDOCK hide #-}

-- Fast integer logarithms to base 2.
-- integerLog2# and wordLog2# are of general usefulness,
-- the others are only needed for a fast implementation of
-- fromRational.
-- Since they are needed in GHC.Float, we must expose this
-- module, but it should not show up in the docs.

-- TODO: fix roundingMode!

module GHC.Integer.Logarithms.Internals
    ( integerLog2#
    , integerLog2IsPowerOf2#
    , wordLog2#
    , roundingMode#
    ) where

import GHC.Prim

-- Moved into GHC.Integer.Type to avoid GHC confusion between host and JS
-- versions of Integer.
import GHC.Integer.Type (
    integerLog2#, integerLog2IsPowerOf2#, wordLog2#, roundingMode#
  )

-- When larger word sizes become common, add support for those,
-- it is not hard, just tedious.
default ()
