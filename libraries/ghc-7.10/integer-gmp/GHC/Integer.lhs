\begin{code}
{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Integer
-- Copyright   :  (c) The University of Glasgow 1994-2008
-- License     :  see libraries/integer-gmp/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Integer' type.
--
-----------------------------------------------------------------------------

module GHC.Integer (
    Integer, mkInteger,
    smallInteger, wordToInteger, integerToWord, integerToInt,
    integerToWord64, word64ToInteger,
    integerToInt64, int64ToInteger,
    plusInteger, minusInteger, timesInteger, negateInteger,
    eqInteger, neqInteger, absInteger, signumInteger,
    leInteger, gtInteger, ltInteger, geInteger, compareInteger,
    divModInteger, divInteger, modInteger,
    quotRemInteger, quotInteger, remInteger,
    encodeFloatInteger, floatFromInteger,
    encodeDoubleInteger, decodeDoubleInteger, doubleFromInteger,
    -- gcdInteger, lcmInteger,
    andInteger, orInteger, xorInteger, complementInteger,
    shiftLInteger, shiftRInteger,
    hashInteger, integerToJSString,
    testBitInteger
 ) where

import GHC.Integer.Type

default ()
\end{code}

