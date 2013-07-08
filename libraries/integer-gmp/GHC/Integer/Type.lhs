\begin{code}
{-# LANGUAGE BangPatterns, CPP, MagicHash, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

#define INT_MINBOUND (-2147483648#)
#define NEG_INT_MINBOUND (S# 2147483647# `plusInteger` S# 1#)

module GHC.Integer.Type where

import GHC.Prim (
    -- Other types we use, convert from, or convert to
    Int#, Word#, Double#, Float#, ByteArray#,
    -- Conversions between those types
    int2Word#, int2Double#, int2Float#, word2Int#,
    -- Operations on Int# that we use for operations on S#
    quotInt#, remInt#, negateInt#,
    (==#), (/=#), (<=#), (>=#), (<#), (>#), (*#), (-#),
    mulIntMayOflo#, addIntC#, subIntC#,
    and#, or#, xor#, powerFloat#, timesFloat#, (*##), (**##)
 )

import GHC.Integer.GMP.Prim (
    -- GMP-related primitives
    cmpInteger#, cmpIntegerInt#,
    plusInteger#, minusInteger#, timesInteger#,
    quotRemInteger#, quotInteger#, remInteger#,
    divModInteger#, divInteger#, modInteger#,
    decodeDouble#,
    int2Integer#, integer2Int#, word2Integer#, integer2Word#,
    andInteger#, orInteger#, xorInteger#, complementInteger#,
    int64ToInteger#,  integerToInt64#,
    word64ToInteger#, integerToWord64#,
    shiftLInteger#, shiftRInteger#, toFloat#, toDouble#,
    negateInteger#, integerToJSString#, fromRat#
 )

import GHC.IntWord64 (
            Int64#, Word64#,
            int64ToWord64#, intToInt64#,
            int64ToInt#, word64ToInt64#,
            geInt64#, leInt64#, leWord64#,
       )

import GHC.Classes
import GHC.Types

default ()
\end{code}

%*********************************************************
%*                                                      *
\subsection{The @Integer@ type}
%*                                                      *
%*********************************************************

Convenient boxed Integer PrimOps.

\begin{code}
-- | Arbitrary-precision integers.
data Integer
   = S# Int#                       -- small integers
   | J# ByteArray#                 -- large integers

mkInteger :: Bool   -- non-negative?
          -> [Int]  -- absolute value in 31 bit chunks, least significant first
                    -- ideally these would be Words rather than Ints, but
                    -- we don't have Word available at the moment.
          -> Integer
mkInteger nonNegative is = let abs = f is
                           in if nonNegative then abs else negateInteger abs
    where f [] = S# 0#
          f (I# i : is') = S# i `orInteger` shiftLInteger (f is') 31#

{-# NOINLINE smallInteger #-}
smallInteger :: Int# -> Integer
smallInteger i = S# i

{-# NOINLINE wordToInteger #-}
wordToInteger :: Word# -> Integer
wordToInteger w = J# (word2Integer# w)

{-# RULES
"integerToInt/smallInteger"   forall x . integerToInt  (smallInteger  x) = x
"integerToWord/wordToInteger" forall x . integerToWord (wordToInteger x) = x
 #-}

{-# NOINLINE integerToWord #-}
integerToWord :: Integer -> Word#
integerToWord (S# i) = int2Word# i
integerToWord (J# n) = integer2Word# n

{-# NOINLINE integerToWord64 #-}
integerToWord64 :: Integer -> Word64#
integerToWord64 (S# i) = int64ToWord64# (intToInt64# i)
integerToWord64 (J# n) = integerToWord64# n

{-# NOINLINE word64ToInteger #-}
word64ToInteger :: Word64# -> Integer
word64ToInteger w = if w `leWord64#` int64ToWord64# (intToInt64# 0x7FFFFFFF#)
                    then S# (int64ToInt# (word64ToInt64# w))
                    else J# (word64ToInteger# w)

{-# NOINLINE integerToInt64 #-}
integerToInt64 :: Integer -> Int64#
integerToInt64 (S# i) = intToInt64# i
integerToInt64 (J# n) = integerToInt64# n

{-# NOINLINE int64ToInteger #-}
int64ToInteger :: Int64# -> Integer
int64ToInteger i = if ((i `leInt64#` intToInt64# 0x7FFFFFFF#) &&
                       (i `geInt64#` intToInt64# -0x80000000#))
                   then smallInteger (int64ToInt# i)
                   else J# (int64ToInteger# i)

{-# RULES
"integerToInt64/int64ToInteger"   forall x . integerToInt64  (int64ToInteger  x) = x
"integerToWord64/word64ToInteger" forall x . integerToWord64 (word64ToInteger x) = x
 #-}

integerToInt :: Integer -> Int#
{-# NOINLINE integerToInt #-}
integerToInt (S# i)   = i
integerToInt (J# n) = integer2Int# n

toBig :: Integer -> Integer
toBig (S# i)   = J# (int2Integer# i)
toBig i@(J# _) = i
\end{code}


%*********************************************************
%*                                                      *
\subsection{Dividing @Integers@}
%*                                                      *
%*********************************************************

\begin{code}
-- XXX There's no good reason for us using unboxed tuples for the
-- results, but we don't have Data.Tuple available.

-- Note that we don't check for divide-by-zero here. That needs
-- to be done where it's used.
-- (we don't have error)

{-# NOINLINE quotRemInteger #-}
quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger a@(S# INT_MINBOUND) b = quotRemInteger (toBig a) b
quotRemInteger (S# i) (S# j) = (# S# q, S# r #)
    where
      -- NB. don't inline these.  (# S# (i `quotInt#` j), ... #) means
      -- (# let q = i `quotInt#` j in S# q, ... #) which builds a
      -- useless thunk.  Placing the bindings here means they'll be
      -- evaluated strictly.
      !q = i `quotInt#` j
      !r = i `remInt#`  j
quotRemInteger i1@(J# _) i2@(S# _) = quotRemInteger i1 (toBig i2)
quotRemInteger i1@(S# _) i2@(J# _) = quotRemInteger (toBig i1) i2
quotRemInteger (J# n1) (J# n2)
  = case (quotRemInteger# n1 n2) of
          (# n3, n4 #)
            -> (# J# n3, J# n4 #)

{-# NOINLINE divModInteger #-}
divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger a@(S# INT_MINBOUND) b = divModInteger (toBig a) b
divModInteger (S# i) (S# j) = (# S# d, S# m #)
    where
      -- NB. don't inline these.  See quotRemInteger above.
      !d = i `divInt#` j
      !m = i `modInt#` j

divModInteger i1@(J# _) i2@(S# _) = divModInteger i1 (toBig i2)
divModInteger i1@(S# _) i2@(J# _) = divModInteger (toBig i1) i2
divModInteger (J# n1) (J# n2)
  = case (divModInteger# n1 n2) of
          (# n3, n4 #)
            -> (# J# n3, J# n4 #)

{-# NOINLINE remInteger #-}
remInteger :: Integer -> Integer -> Integer
remInteger a@(S# INT_MINBOUND) b = remInteger (toBig a) b
remInteger (S# a) (S# b) = S# (remInt# a b)
{- Special case doesn't work, because a 1-element J# has the range
   -(2^32-1) -- 2^32-1, whereas S# has the range -2^31 -- (2^31-1)
remInteger ia@(S# a) (J# sb b)
  | sb ==# 1#  = S# (remInt# a (word2Int# (integer2Word# sb b)))
  | sb ==# -1# = S# (remInt# a (0# -# (word2Int# (integer2Word# sb b))))
  | 0# <# sb   = ia
  | otherwise  = S# (0# -# a)
-}
remInteger ia@(S# _) ib@(J# _) = remInteger (toBig ia) ib
remInteger (J# a) (S# b)
  = S# (integer2Int# (remInteger# a (int2Integer# b)))
remInteger (J# a) (J# b)
  = J# (remInteger# a b)

{-# NOINLINE quotInteger #-}
quotInteger :: Integer -> Integer -> Integer
quotInteger a@(S# INT_MINBOUND) b = quotInteger (toBig a) b
quotInteger (S# a) (S# b) = S# (quotInt# a b)
{- Special case disabled, see remInteger above
quotInteger (S# a) (J# sb b)
  | sb ==# 1#  = S# (quotInt# a (word2Int# (integer2Word# sb b)))
  | sb ==# -1# = S# (quotInt# a (0# -# (word2Int# (integer2Word# sb b))))
  | otherwise  = S# 0
-}
quotInteger ia@(S# _) ib@(J# _) = quotInteger (toBig ia) ib
quotInteger (J# a) (S# b)
  = S# (integer2Int# (quotInteger# a (int2Integer# b)))
quotInteger (J# a) (J# b)
  = J# (quotInteger# a b)

{-# NOINLINE modInteger #-}
modInteger :: Integer -> Integer -> Integer
modInteger a@(S# INT_MINBOUND) b = modInteger (toBig a) b
modInteger (S# a) (S# b) = S# (modInt# a b)
modInteger ia@(S# _) ib@(J# _) = modInteger (toBig ia) ib
modInteger (J# a) (S# b)
  = S# (integer2Int# (modInteger# a (int2Integer# b)))
modInteger (J# a) (J# b)
  = J# (modInteger# a b)

{-# NOINLINE divInteger #-}
divInteger :: Integer -> Integer -> Integer
divInteger a@(S# INT_MINBOUND) b = divInteger (toBig a) b
divInteger (S# a) (S# b) = S# (divInt# a b)
divInteger ia@(S# _) ib@(J# _) = divInteger (toBig ia) ib
divInteger (J# a) (S# b)
  = S# (integer2Int# (divInteger# a (int2Integer# b)))
divInteger (J# a) (J# b)
  = J# (divInteger# a b)
\end{code}



\begin{code}
-- We can't throw an error here, so it is up to our caller to
-- not call us with both arguments being 0.
{-# NOINLINE gcdInteger #-}
gcdInteger :: Integer -> Integer -> Integer
gcdInteger a b =
    gcdI (absInteger a) (absInteger b)
  where
    gcdI a b = if b `eqInteger` S# 0#
                 then a
                 else gcdI b (a `remInteger` b)

{-# NOINLINE lcmInteger #-}
lcmInteger :: Integer -> Integer -> Integer
lcmInteger a b =      if a `eqInteger` S# 0# then S# 0#
                 else if b `eqInteger` S# 0# then S# 0#
                 else absInteger ((a `quotInteger` (gcdInteger a b))
                                  `timesInteger` b)
\end{code}



%*********************************************************
%*                                                      *
\subsection{The @Integer@ instances for @Eq@, @Ord@}
%*                                                      *
%*********************************************************

\begin{code}
{-# NOINLINE eqInteger #-}
eqInteger :: Integer -> Integer -> Bool
eqInteger (S# i)     (S# j) = i ==# j
eqInteger (S# i)     (J# d) = cmpIntegerInt# d i ==# 0#
eqInteger (J# d)   (S# i)   = cmpIntegerInt# d i ==# 0#
eqInteger (J# a) (J# b)     = (cmpInteger# a b) ==# 0#

{-# NOINLINE neqInteger #-}
neqInteger :: Integer -> Integer -> Bool
neqInteger (S# i)     (S# j) = i /=# j
neqInteger (S# i)     (J# d) = cmpIntegerInt# d i /=# 0#
neqInteger (J# d)     (S# i) = cmpIntegerInt# d i /=# 0#
neqInteger (J# a) (J# b)     = (cmpInteger# a b) /=# 0#

instance  Eq Integer  where
    (==) = eqInteger
    (/=) = neqInteger

------------------------------------------------------------------------

{-# NOINLINE leInteger #-}
leInteger :: Integer -> Integer -> Bool
leInteger (S# i)     (S# j) = i <=# j
leInteger (J# d)   (S# i)   = cmpIntegerInt# d i <=# 0#
leInteger (S# i)     (J# d) = cmpIntegerInt# d i >=# 0#
leInteger (J# a) (J# b)     = (cmpInteger# a b) <=# 0#

{-# NOINLINE gtInteger #-}
gtInteger :: Integer -> Integer -> Bool
gtInteger (S# i)     (S# j) = i ># j
gtInteger (J# d)   (S# i)   = cmpIntegerInt# d i ># 0#
gtInteger (S# i)     (J# d) = cmpIntegerInt# d i <# 0#
gtInteger (J# a) (J# b)     = (cmpInteger# a b) ># 0#

{-# NOINLINE ltInteger #-}
ltInteger :: Integer -> Integer -> Bool
ltInteger (S# i)     (S# j) = i <# j
ltInteger (J# d)   (S# i)   = cmpIntegerInt# d i <# 0#
ltInteger (S# i)     (J# d) = cmpIntegerInt# d i ># 0#
ltInteger (J# a) (J# b)     = (cmpInteger# a b) <# 0#

{-# NOINLINE geInteger #-}
geInteger :: Integer -> Integer -> Bool
geInteger (S# i)     (S# j)     = i >=# j
geInteger (J# d)   (S# i)   = cmpIntegerInt# d i >=# 0#
geInteger (S# i)     (J# d) = cmpIntegerInt# d i <=# 0#
geInteger (J# a) (J# b)     = (cmpInteger# a b) >=# 0#

{-# NOINLINE compareInteger #-}
compareInteger :: Integer -> Integer -> Ordering
compareInteger (S# i)  (S# j)
   =      if i ==# j then EQ
     else if i <=# j then LT
     else                 GT
compareInteger (J# d) (S# i)
   = case cmpIntegerInt# d i of { res# ->
     if res# <# 0# then LT else
     if res# ># 0# then GT else EQ
     }
compareInteger (S# i) (J# d)
   = case cmpIntegerInt# d i of { res# ->
     if res# ># 0# then LT else
     if res# <# 0# then GT else EQ
     }
compareInteger (J# a) (J# b)
   = case cmpInteger# a b of { res# ->
     if res# <# 0# then LT else
     if res# ># 0# then GT else EQ
     }

instance Ord Integer where
    (<=) = leInteger
    (>)  = gtInteger
    (<)  = ltInteger
    (>=) = geInteger
    compare = compareInteger
\end{code}


%*********************************************************
%*                                                      *
\subsection{The @Integer@ instances for @Num@}
%*                                                      *
%*********************************************************

\begin{code}
{-# NOINLINE absInteger #-}
absInteger :: Integer -> Integer
absInteger (S# INT_MINBOUND) = NEG_INT_MINBOUND
absInteger n@(S# i) = if i >=# 0# then n else S# (negateInt# i)
absInteger n@(J# d) = if cmpIntegerInt# d 0# <# 0#
                        then J# (negateInteger# d)
                        else n

{-# NOINLINE signumInteger #-}
signumInteger :: Integer -> Integer
signumInteger (S# i) = if i <# 0# then S# -1#
                       else if i ==# 0# then S# 0#
                       else S# 1#
signumInteger (J# d)
  = let
        !cmp = cmpIntegerInt# d 0#
    in
    if      cmp >#  0# then S# 1#
    else if cmp ==# 0# then S# 0#
    else                    S# (negateInt# 1#)

{-# NOINLINE plusInteger #-}
plusInteger :: Integer -> Integer -> Integer
plusInteger i1@(S# i) i2@(S# j)  = case addIntC# i j of
                                   (# r, c #) ->
                                       if c ==# 0#
                                       then S# r
                                       else plusInteger (toBig i1) (toBig i2)
plusInteger i1@(J# _) i2@(S# _) = plusInteger i1 (toBig i2)
plusInteger i1@(S# _) i2@(J# _) = plusInteger (toBig i1) i2
plusInteger (J# a) (J# b)       = J# (plusInteger# a b)

{-# NOINLINE minusInteger #-}
minusInteger :: Integer -> Integer -> Integer
minusInteger i1@(S# i) i2@(S# j)   = case subIntC# i j of
                                     (# r, c #) ->
                                         if c ==# 0# then S# r
                                         else minusInteger (toBig i1)
                                                           (toBig i2)
minusInteger i1@(J# _) i2@(S# _) = minusInteger i1 (toBig i2)
minusInteger i1@(S# _) i2@(J# _) = minusInteger (toBig i1) i2
minusInteger (J# a) (J# b)       = J# (minusInteger# a b)

{-# NOINLINE timesInteger #-}
timesInteger :: Integer -> Integer -> Integer
timesInteger i1@(S# i) i2@(S# j)   = if mulIntMayOflo# i j ==# 0#
                                     then S# (i *# j)
                                     else timesInteger (toBig i1) (toBig i2)
timesInteger i1@(J# _) i2@(S# _) = timesInteger i1 (toBig i2)
timesInteger i1@(S# _) i2@(J# _) = timesInteger (toBig i1) i2
timesInteger (J# a) (J# b)       = J# (timesInteger# a b)

{-# NOINLINE negateInteger #-}
negateInteger :: Integer -> Integer
negateInteger (S# INT_MINBOUND) = NEG_INT_MINBOUND
negateInteger (S# i)            = S# (negateInt# i)
negateInteger (J# i)            = J# (negateInteger# i)
\end{code}


%*********************************************************
%*                                                      *
\subsection{The @Integer@ stuff for Double@}
%*                                                      *
%*********************************************************

\begin{code}
{-# NOINLINE encodeFloatInteger #-}
encodeFloatInteger :: Integer -> Int# -> Float#
encodeFloatInteger (S# m) e =
  int2Float# m `timesFloat#` (2.0# `powerFloat#` (int2Float# e))
encodeFloatInteger (J# m) e =
  toFloat# m `timesFloat#` (2.0# `powerFloat#` (int2Float# e))

{-# NOINLINE encodeDoubleInteger #-}
encodeDoubleInteger :: Integer -> Int# -> Double#
encodeDoubleInteger (S# m) e =
  int2Double# m *## (2.0## **## (int2Double# e))
encodeDoubleInteger (J# m) e =
  toDouble# m *## (2.0## **## (int2Double# e))

{-# NOINLINE decodeDoubleInteger #-}
decodeDoubleInteger :: Double# -> (# Integer, Int# #)
decodeDoubleInteger d = case decodeDouble# d of
                        (# exp#, d# #) -> (# J# d#, exp# #)

-- previous code: doubleFromInteger n = fromInteger n = encodeFloat n 0
-- doesn't work too well, because encodeFloat is defined in
-- terms of ccalls which can never be simplified away.  We
-- want simple literals like (fromInteger 3 :: Float) to turn
-- into (F# 3.0), hence the special case for S# here.

{-# NOINLINE doubleFromInteger #-}
doubleFromInteger :: Integer -> Double#
doubleFromInteger (S# i#) = int2Double# i#
doubleFromInteger (J# i#) = toDouble# i#

{-# NOINLINE floatFromInteger #-}
floatFromInteger :: Integer -> Float#
floatFromInteger (S# i#) = int2Float# i#
floatFromInteger (J# i#) = toFloat# i#

\end{code}

%*********************************************************
%*                                                      *
\subsection{The @Integer@ Bit definitions@}
%*                                                      *
%*********************************************************

We explicitly pattern match against J# and S# in order to produce
Core that doesn't have pattern matching errors, as that would
introduce a spurious dependency to base.

\begin{code}
{-# NOINLINE andInteger #-}
andInteger :: Integer -> Integer -> Integer
(S# x) `andInteger` (S# y) = S# (word2Int# (int2Word# x `and#` int2Word# y))
x@(S# _) `andInteger` y@(J# _) = toBig x `andInteger` y
x@(J# _) `andInteger` y@(S# _) = x `andInteger` toBig y
(J# a) `andInteger` (J# b)     = J# (andInteger# a b)

{-# NOINLINE orInteger #-}
orInteger :: Integer -> Integer -> Integer
(S# x) `orInteger` (S# y) = S# (word2Int# (int2Word# x `or#` int2Word# y))
x@(S# _) `orInteger` y@(J# _) = toBig x `orInteger` y
x@(J# _) `orInteger` y@(S# _) = x `orInteger` toBig y
(J# a) `orInteger` (J# b)     = J# (orInteger# a b)

{-# NOINLINE xorInteger #-}
xorInteger :: Integer -> Integer -> Integer
(S# x) `xorInteger` (S# y) = S# (word2Int# (int2Word# x `xor#` int2Word# y))
x@(S# _) `xorInteger` y@(J# _) = toBig x `xorInteger` y
x@(J# _) `xorInteger` y@(S# _) = x `xorInteger` toBig y
(J# a) `xorInteger` (J# b)     = J# (xorInteger# a b)

{-# NOINLINE complementInteger #-}
complementInteger :: Integer -> Integer
complementInteger (S# x)
    = S# (word2Int# (int2Word# x `xor#` 0xffffffff##))
complementInteger (J# d)
    = J# (complementInteger# d)

{-# NOINLINE shiftLInteger #-}
shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger j@(S# _) i = shiftLInteger (toBig j) i
shiftLInteger (J# d) i   = J# (shiftLInteger# d i)

{-# NOINLINE shiftRInteger #-}
shiftRInteger :: Integer -> Int# -> Integer
shiftRInteger j@(S# _) i = shiftRInteger (toBig j) i
shiftRInteger (J# d) i   = J# (shiftRInteger# d i)

{-# NOINLINE integerToJSString #-}
integerToJSString :: Integer -> ByteArray#
integerToJSString i@(S# _) = integerToJSString (toBig i)
integerToJSString (J# i)   = integerToJSString# i

-- Convert a/b into a Double
fromRat :: Integer -> Integer -> Double
fromRat i@(S# _) j    = fromRat (toBig i) j
fromRat i j@(S# _)    = fromRat i (toBig j)
fromRat (J# i) (J# j) = D# (fromRat# i j)

\end{code}

%*********************************************************
%*                                                      *
\subsection{The @Integer@ hashing@}
%*                                                      *
%*********************************************************

\begin{code}
-- This is used by hashUnique

-- | hashInteger returns the same value as 'fromIntegral', although in
-- unboxed form.  It might be a reasonable hash function for 'Integer', 
-- given a suitable distribution of 'Integer' values.

hashInteger :: Integer -> Int#
hashInteger = integerToInt
\end{code}

