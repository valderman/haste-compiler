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
import GHC.Integer.Type

-- When larger word sizes become common, add support for those,
-- it is not hard, just tedious.
default ()

#define WSHIFT 5
#define MMASK 31

-- Assumption: Integer is strictly positive
-- For small integers, use wordLog#,
-- in the general case, check words from the most
-- significant down, once a nonzero word is found,
-- calculate its log2 and add the number of following bits.
integerLog2# :: Integer -> Int#
integerLog2# (S# i) = wordLog2# (int2Word# i)
integerLog2# (J# ba) = log2I# ba

foreign import prim "I_log2"
  log2I# :: ByteArray# -> Int#

-- Assumption: Integer is strictly positive
-- First component is log2 n, second is 0# iff n is a power of two
integerLog2IsPowerOf2# :: Integer -> (# Int#, Int# #)
-- The power of 2 test is n&(n-1) == 0, thus powers of 2
-- are indicated bythe second component being zero.
integerLog2IsPowerOf2# (S# i) =
    case int2Word# i of
      w -> (# wordLog2# w, word2Int# (w `and#` (w `minusWord#` 1##)) #)
-- Find the log2 as above, test whether that word is a power
-- of 2, if so, check whether only zero bits follow.
integerLog2IsPowerOf2# n@(J# _) =
    case integerLog2# n of
      l -> (# l, if is2pow l then 0# else 1# #)
  where
    is2pow n =
      case wordLog2# (int2Word# n) of 
        l -> l +# l ==# n

-- Assumption: Integer and Int# are strictly positive, Int# is less
-- than logBase 2 of Integer, otherwise havoc ensues.
-- Used only for the numerator in fromRational when the denominator
-- is a power of 2.
-- The Int# argument is log2 n minus the number of bits in the mantissa
-- of the target type, i.e. the index of the first non-integral bit in
-- the quotient.
--
-- 0# means round down (towards zero)
-- 1# means we have a half-integer, round to even
-- 2# means round up (away from zero)
roundingMode# :: Integer -> Int# -> Int#
roundingMode# (S# i) t =
    case int2Word# i `and#` ((uncheckedShiftL# 2## t) `minusWord#` 1##) of
      k -> case uncheckedShiftL# 1## t of
            c -> if c `gtWord#` k
                    then 0#
                    else if c `ltWord#` k
                            then 2#
                            else 1#
roundingMode# (J# ba) t =
    case word2Int# (int2Word# t `and#` MMASK##) of
      j ->      -- index of relevant bit in word
        case uncheckedIShiftRA# t WSHIFT# of
          k ->  -- index of relevant word
            case indexWordArray# ba k `and#`
                    ((uncheckedShiftL# 2## j) `minusWord#` 1##) of
              r ->
                case uncheckedShiftL# 1## j of
                  c -> if c `gtWord#` r
                        then 0#
                        else if c `ltWord#` r
                                then 2#
                                else test (k -# 1#)
  where
    test i = if i <# 0#
                then 1#
                else case indexWordArray# ba i of
                        0## -> test (i -# 1#)
                        _   -> 2#

-- wordLog2# 0## = -1#
{-# INLINE wordLog2# #-}
wordLog2# :: Word# -> Int#
wordLog2# w =
  case leadingZeros of
   BA lz ->
    let zeros u = indexInt8Array# lz (word2Int# u) in
                    case uncheckedShiftRL# w 24# of
                     e ->
                      if e `neWord#` 0##
                       then 32# -# zeros e
                       else
                        case uncheckedShiftRL# w 16# of
                         f ->
                          if f `neWord#` 0##
                           then 24# -# zeros f
                           else
                            case uncheckedShiftRL# w 8# of
                             g ->
                              if g `neWord#` 0##
                               then 16# -# zeros g
                               else 8# -# zeros w


-- Lookup table
data BA = BA ByteArray#

leadingZeros :: BA
leadingZeros =
    let mkArr s =
          case newByteArray# 256# s of
            (# s1, mba #) ->
              case writeInt8Array# mba 0# 9# s1 of
                s2 ->
                  let fillA lim val idx st =
                        if idx ==# 256#
                          then st
                          else if idx <# lim
                                then case writeInt8Array# mba idx val st of
                                        nx -> fillA lim val (idx +# 1#) nx
                                else fillA (2# *# lim) (val -# 1#) idx st
                  in case fillA 2# 8# 1# s2 of
                      s3 -> case unsafeFreezeByteArray# mba s3 of
                              (# _, ba #) -> ba
    in case mkArr realWorld# of
        b -> BA b
