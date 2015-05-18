{-# LANGUAGE ForeignFunctionInterface, GHCForeignImportPrim, CPP,
             MagicHash, UnboxedTuples, UnliftedFFITypes, BangPatterns #-}
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module GHC.Integer.GMP.Prim (
    cmpInteger#,
    cmpIntegerInt#,

    plusInteger#,
    minusInteger#,
    timesInteger#,

    quotRemInteger#,
    quotInteger#,
    remInteger#,
    divModInteger#,
    divInteger#,
    modInteger#,

    decodeDouble#,

    int2Integer#,
    integer2Int#,

    word2Integer#,
    integer2Word#,

    andInteger#,
    orInteger#,
    xorInteger#,
    complementInteger#,
    
    shiftLInteger#,
    shiftRInteger#,

    int64ToInteger#,  integerToInt64#,
    word64ToInteger#, integerToWord64#,
    toFloat#, toDouble#,
    negateInteger#,
    integerToJSString#,
    fromRat#
  ) where

import GHC.Prim

-- Double isn't available yet, and we shouldn't be using defaults anyway:
default ()

-- | Returns -1,0,1 according as first argument is less than, equal to, or greater than second argument.
--
foreign import prim "I_compare" cmpInteger#
  :: ByteArray# -> ByteArray# -> Int#

-- | Returns -1,0,1 according as first argument is less than, equal to, or greater than second argument, which
--   is an ordinary Int\#.
foreign import prim "I_compareInt" cmpIntegerInt#
  :: ByteArray# -> Int# -> Int#

-- |
--
foreign import prim "I_add" plusInteger#
  :: ByteArray# -> ByteArray# -> ByteArray#

-- |
--
foreign import prim "I_sub" minusInteger#
  :: ByteArray# -> ByteArray# -> ByteArray#

-- |
--
foreign import prim "I_mul" timesInteger#
  :: ByteArray# -> ByteArray# -> ByteArray#

-- | Compute div and mod simultaneously, where div rounds towards negative
-- infinity and\ @(q,r) = divModInteger#(x,y)@ implies
-- @plusInteger# (timesInteger# q y) r = x@.
--
foreign import prim "I_quotRem" quotRemInteger#
  :: ByteArray# -> ByteArray# -> (# ByteArray#, ByteArray# #)

-- | Rounds towards zero.
--
foreign import prim "I_quot" quotInteger#
  :: ByteArray# -> ByteArray# -> ByteArray#

-- | Satisfies \texttt{plusInteger\# (timesInteger\# (quotInteger\# x y) y) (remInteger\# x y) == x}.
--
foreign import prim "I_rem" remInteger#
  :: ByteArray# -> ByteArray# -> ByteArray#

-- | Compute div and mod simultaneously, where div rounds towards negative infinity
-- and\texttt{(q,r) = divModInteger\#(x,y)} implies \texttt{plusInteger\# (timesInteger\# q y) r = x}.
--
foreign import prim "I_divMod" divModInteger#
  :: ByteArray# -> ByteArray# -> (# ByteArray#, ByteArray# #)
foreign import prim "I_div" divInteger#
  :: ByteArray# -> ByteArray# -> ByteArray#
foreign import prim "I_mod" modInteger#
  :: ByteArray# -> ByteArray# -> ByteArray#



-------


-- | Convert to arbitrary-precision integer.
--    First {\tt Int\#} in result is the exponent; second {\tt Int\#} and {\tt ByteArray\#}
--  represent an {\tt Integer\#} holding the mantissa.
--
foreign import prim "I_decodeDouble" decodeDouble#
  :: Double# -> (# Int#, ByteArray# #)

-- |
--
foreign import prim "I_fromInt" int2Integer#
  :: Int# -> ByteArray#

-- |
--
foreign import prim "I_fromInt" word2Integer#
  :: Word# -> ByteArray#

-- |
--
foreign import prim "I_and" andInteger#
  :: ByteArray# -> ByteArray# -> ByteArray#

-- |
--
foreign import prim "I_or" orInteger#
  :: ByteArray# -> ByteArray# -> ByteArray#

-- |
--
foreign import prim "I_xor" xorInteger#
  :: ByteArray# -> ByteArray# -> ByteArray#

-- |
--
foreign import prim "I_complement" complementInteger#
  :: ByteArray# -> ByteArray#

foreign import prim "I_fromInt64" int64ToInteger#
  :: Int64# -> ByteArray#

foreign import prim "I_fromWord64" word64ToInteger#
  :: Word64# -> ByteArray#

foreign import prim "I_toInt64"
    integerToInt64#  :: ByteArray# -> Int64#

foreign import prim "I_toWord64"
    integerToWord64# :: ByteArray# -> Word64#

foreign import prim "I_toInt"
    integer2Int# :: ByteArray# -> Int#

foreign import prim "I_negate"
    negateInteger# :: ByteArray# -> ByteArray#

foreign import prim "I_abs"
    absInteger# :: ByteArray# -> ByteArray#

foreign import prim "I_toNumber"
    toDouble# :: ByteArray# -> Double#

foreign import prim "I_toNumber"
    toFloat# :: ByteArray# -> Float#

foreign import prim "I_shiftLeft"
    shiftLInteger# :: ByteArray# -> Int# -> ByteArray#

foreign import prim "I_shiftRight"
    shiftRInteger# :: ByteArray# -> Int# -> ByteArray#

foreign import prim "I_toString"
    integerToJSString# :: ByteArray# -> ByteArray#

foreign import prim "I_fromRat"
    fromRat# :: ByteArray# -> ByteArray# -> Double#

integer2Word# :: ByteArray# -> Word#
integer2Word# n = int2Word# (integer2Int# n)
