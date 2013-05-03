{-# LANGUAGE NoImplicitPrelude, UnboxedTuples #-}
module GHC.PrimopWrappers where
import qualified GHC.Prim
import GHC.Types (Bool)
import GHC.Tuple ()
import GHC.Prim (Char#, Int#, Word#, Float#, Double#, State#, MutableArray#, Array#, MutableByteArray#, ByteArray#, Addr#, StablePtr#, MutableArrayArray#, ArrayArray#, MutVar#, RealWorld, TVar#, MVar#, ThreadId#, Weak#, StableName#, BCO#)
{-# NOINLINE gtChar# #-}
gtChar# :: Char# -> Char# -> Bool
gtChar# a1 a2 = (GHC.Prim.gtChar#) a1 a2
{-# NOINLINE geChar# #-}
geChar# :: Char# -> Char# -> Bool
geChar# a1 a2 = (GHC.Prim.geChar#) a1 a2
{-# NOINLINE eqChar# #-}
eqChar# :: Char# -> Char# -> Bool
eqChar# a1 a2 = (GHC.Prim.eqChar#) a1 a2
{-# NOINLINE neChar# #-}
neChar# :: Char# -> Char# -> Bool
neChar# a1 a2 = (GHC.Prim.neChar#) a1 a2
{-# NOINLINE ltChar# #-}
ltChar# :: Char# -> Char# -> Bool
ltChar# a1 a2 = (GHC.Prim.ltChar#) a1 a2
{-# NOINLINE leChar# #-}
leChar# :: Char# -> Char# -> Bool
leChar# a1 a2 = (GHC.Prim.leChar#) a1 a2
{-# NOINLINE ord# #-}
ord# :: Char# -> Int#
ord# a1 = (GHC.Prim.ord#) a1
{-# NOINLINE (+#) #-}
(+#) :: Int# -> Int# -> Int#
(+#) a1 a2 = (GHC.Prim.+#) a1 a2
{-# NOINLINE (-#) #-}
(-#) :: Int# -> Int# -> Int#
(-#) a1 a2 = (GHC.Prim.-#) a1 a2
{-# NOINLINE (*#) #-}
(*#) :: Int# -> Int# -> Int#
(*#) a1 a2 = (GHC.Prim.*#) a1 a2
{-# NOINLINE mulIntMayOflo# #-}
mulIntMayOflo# :: Int# -> Int# -> Int#
mulIntMayOflo# a1 a2 = (GHC.Prim.mulIntMayOflo#) a1 a2
{-# NOINLINE quotInt# #-}
quotInt# :: Int# -> Int# -> Int#
quotInt# a1 a2 = (GHC.Prim.quotInt#) a1 a2
{-# NOINLINE remInt# #-}
remInt# :: Int# -> Int# -> Int#
remInt# a1 a2 = (GHC.Prim.remInt#) a1 a2
{-# NOINLINE quotRemInt# #-}
quotRemInt# :: Int# -> Int# -> (# Int#,Int# #)
quotRemInt# a1 a2 = (GHC.Prim.quotRemInt#) a1 a2
{-# NOINLINE negateInt# #-}
negateInt# :: Int# -> Int#
negateInt# a1 = (GHC.Prim.negateInt#) a1
{-# NOINLINE addIntC# #-}
addIntC# :: Int# -> Int# -> (# Int#,Int# #)
addIntC# a1 a2 = (GHC.Prim.addIntC#) a1 a2
{-# NOINLINE subIntC# #-}
subIntC# :: Int# -> Int# -> (# Int#,Int# #)
subIntC# a1 a2 = (GHC.Prim.subIntC#) a1 a2
{-# NOINLINE (>#) #-}
(>#) :: Int# -> Int# -> Bool
(>#) a1 a2 = (GHC.Prim.>#) a1 a2
{-# NOINLINE (>=#) #-}
(>=#) :: Int# -> Int# -> Bool
(>=#) a1 a2 = (GHC.Prim.>=#) a1 a2
{-# NOINLINE (==#) #-}
(==#) :: Int# -> Int# -> Bool
(==#) a1 a2 = (GHC.Prim.==#) a1 a2
{-# NOINLINE (/=#) #-}
(/=#) :: Int# -> Int# -> Bool
(/=#) a1 a2 = (GHC.Prim./=#) a1 a2
{-# NOINLINE (<#) #-}
(<#) :: Int# -> Int# -> Bool
(<#) a1 a2 = (GHC.Prim.<#) a1 a2
{-# NOINLINE (<=#) #-}
(<=#) :: Int# -> Int# -> Bool
(<=#) a1 a2 = (GHC.Prim.<=#) a1 a2
{-# NOINLINE chr# #-}
chr# :: Int# -> Char#
chr# a1 = (GHC.Prim.chr#) a1
{-# NOINLINE int2Word# #-}
int2Word# :: Int# -> Word#
int2Word# a1 = (GHC.Prim.int2Word#) a1
{-# NOINLINE int2Float# #-}
int2Float# :: Int# -> Float#
int2Float# a1 = (GHC.Prim.int2Float#) a1
{-# NOINLINE int2Double# #-}
int2Double# :: Int# -> Double#
int2Double# a1 = (GHC.Prim.int2Double#) a1
{-# NOINLINE uncheckedIShiftL# #-}
uncheckedIShiftL# :: Int# -> Int# -> Int#
uncheckedIShiftL# a1 a2 = (GHC.Prim.uncheckedIShiftL#) a1 a2
{-# NOINLINE uncheckedIShiftRA# #-}
uncheckedIShiftRA# :: Int# -> Int# -> Int#
uncheckedIShiftRA# a1 a2 = (GHC.Prim.uncheckedIShiftRA#) a1 a2
{-# NOINLINE uncheckedIShiftRL# #-}
uncheckedIShiftRL# :: Int# -> Int# -> Int#
uncheckedIShiftRL# a1 a2 = (GHC.Prim.uncheckedIShiftRL#) a1 a2
{-# NOINLINE plusWord# #-}
plusWord# :: Word# -> Word# -> Word#
plusWord# a1 a2 = (GHC.Prim.plusWord#) a1 a2
{-# NOINLINE plusWord2# #-}
plusWord2# :: Word# -> Word# -> (# Word#,Word# #)
plusWord2# a1 a2 = (GHC.Prim.plusWord2#) a1 a2
{-# NOINLINE minusWord# #-}
minusWord# :: Word# -> Word# -> Word#
minusWord# a1 a2 = (GHC.Prim.minusWord#) a1 a2
{-# NOINLINE timesWord# #-}
timesWord# :: Word# -> Word# -> Word#
timesWord# a1 a2 = (GHC.Prim.timesWord#) a1 a2
{-# NOINLINE timesWord2# #-}
timesWord2# :: Word# -> Word# -> (# Word#,Word# #)
timesWord2# a1 a2 = (GHC.Prim.timesWord2#) a1 a2
{-# NOINLINE quotWord# #-}
quotWord# :: Word# -> Word# -> Word#
quotWord# a1 a2 = (GHC.Prim.quotWord#) a1 a2
{-# NOINLINE remWord# #-}
remWord# :: Word# -> Word# -> Word#
remWord# a1 a2 = (GHC.Prim.remWord#) a1 a2
{-# NOINLINE quotRemWord# #-}
quotRemWord# :: Word# -> Word# -> (# Word#,Word# #)
quotRemWord# a1 a2 = (GHC.Prim.quotRemWord#) a1 a2
{-# NOINLINE quotRemWord2# #-}
quotRemWord2# :: Word# -> Word# -> Word# -> (# Word#,Word# #)
quotRemWord2# a1 a2 a3 = (GHC.Prim.quotRemWord2#) a1 a2 a3
{-# NOINLINE and# #-}
and# :: Word# -> Word# -> Word#
and# a1 a2 = (GHC.Prim.and#) a1 a2
{-# NOINLINE or# #-}
or# :: Word# -> Word# -> Word#
or# a1 a2 = (GHC.Prim.or#) a1 a2
{-# NOINLINE xor# #-}
xor# :: Word# -> Word# -> Word#
xor# a1 a2 = (GHC.Prim.xor#) a1 a2
{-# NOINLINE not# #-}
not# :: Word# -> Word#
not# a1 = (GHC.Prim.not#) a1
{-# NOINLINE uncheckedShiftL# #-}
uncheckedShiftL# :: Word# -> Int# -> Word#
uncheckedShiftL# a1 a2 = (GHC.Prim.uncheckedShiftL#) a1 a2
{-# NOINLINE uncheckedShiftRL# #-}
uncheckedShiftRL# :: Word# -> Int# -> Word#
uncheckedShiftRL# a1 a2 = (GHC.Prim.uncheckedShiftRL#) a1 a2
{-# NOINLINE word2Int# #-}
word2Int# :: Word# -> Int#
word2Int# a1 = (GHC.Prim.word2Int#) a1
{-# NOINLINE gtWord# #-}
gtWord# :: Word# -> Word# -> Bool
gtWord# a1 a2 = (GHC.Prim.gtWord#) a1 a2
{-# NOINLINE geWord# #-}
geWord# :: Word# -> Word# -> Bool
geWord# a1 a2 = (GHC.Prim.geWord#) a1 a2
{-# NOINLINE eqWord# #-}
eqWord# :: Word# -> Word# -> Bool
eqWord# a1 a2 = (GHC.Prim.eqWord#) a1 a2
{-# NOINLINE neWord# #-}
neWord# :: Word# -> Word# -> Bool
neWord# a1 a2 = (GHC.Prim.neWord#) a1 a2
{-# NOINLINE ltWord# #-}
ltWord# :: Word# -> Word# -> Bool
ltWord# a1 a2 = (GHC.Prim.ltWord#) a1 a2
{-# NOINLINE leWord# #-}
leWord# :: Word# -> Word# -> Bool
leWord# a1 a2 = (GHC.Prim.leWord#) a1 a2
{-# NOINLINE popCnt8# #-}
popCnt8# :: Word# -> Word#
popCnt8# a1 = (GHC.Prim.popCnt8#) a1
{-# NOINLINE popCnt16# #-}
popCnt16# :: Word# -> Word#
popCnt16# a1 = (GHC.Prim.popCnt16#) a1
{-# NOINLINE popCnt32# #-}
popCnt32# :: Word# -> Word#
popCnt32# a1 = (GHC.Prim.popCnt32#) a1
{-# NOINLINE popCnt64# #-}
popCnt64# :: Word# -> Word#
popCnt64# a1 = (GHC.Prim.popCnt64#) a1
{-# NOINLINE popCnt# #-}
popCnt# :: Word# -> Word#
popCnt# a1 = (GHC.Prim.popCnt#) a1
{-# NOINLINE narrow8Int# #-}
narrow8Int# :: Int# -> Int#
narrow8Int# a1 = (GHC.Prim.narrow8Int#) a1
{-# NOINLINE narrow16Int# #-}
narrow16Int# :: Int# -> Int#
narrow16Int# a1 = (GHC.Prim.narrow16Int#) a1
{-# NOINLINE narrow32Int# #-}
narrow32Int# :: Int# -> Int#
narrow32Int# a1 = (GHC.Prim.narrow32Int#) a1
{-# NOINLINE narrow8Word# #-}
narrow8Word# :: Word# -> Word#
narrow8Word# a1 = (GHC.Prim.narrow8Word#) a1
{-# NOINLINE narrow16Word# #-}
narrow16Word# :: Word# -> Word#
narrow16Word# a1 = (GHC.Prim.narrow16Word#) a1
{-# NOINLINE narrow32Word# #-}
narrow32Word# :: Word# -> Word#
narrow32Word# a1 = (GHC.Prim.narrow32Word#) a1
{-# NOINLINE (>##) #-}
(>##) :: Double# -> Double# -> Bool
(>##) a1 a2 = (GHC.Prim.>##) a1 a2
{-# NOINLINE (>=##) #-}
(>=##) :: Double# -> Double# -> Bool
(>=##) a1 a2 = (GHC.Prim.>=##) a1 a2
{-# NOINLINE (==##) #-}
(==##) :: Double# -> Double# -> Bool
(==##) a1 a2 = (GHC.Prim.==##) a1 a2
{-# NOINLINE (/=##) #-}
(/=##) :: Double# -> Double# -> Bool
(/=##) a1 a2 = (GHC.Prim./=##) a1 a2
{-# NOINLINE (<##) #-}
(<##) :: Double# -> Double# -> Bool
(<##) a1 a2 = (GHC.Prim.<##) a1 a2
{-# NOINLINE (<=##) #-}
(<=##) :: Double# -> Double# -> Bool
(<=##) a1 a2 = (GHC.Prim.<=##) a1 a2
{-# NOINLINE (+##) #-}
(+##) :: Double# -> Double# -> Double#
(+##) a1 a2 = (GHC.Prim.+##) a1 a2
{-# NOINLINE (-##) #-}
(-##) :: Double# -> Double# -> Double#
(-##) a1 a2 = (GHC.Prim.-##) a1 a2
{-# NOINLINE (*##) #-}
(*##) :: Double# -> Double# -> Double#
(*##) a1 a2 = (GHC.Prim.*##) a1 a2
{-# NOINLINE (/##) #-}
(/##) :: Double# -> Double# -> Double#
(/##) a1 a2 = (GHC.Prim./##) a1 a2
{-# NOINLINE negateDouble# #-}
negateDouble# :: Double# -> Double#
negateDouble# a1 = (GHC.Prim.negateDouble#) a1
{-# NOINLINE double2Int# #-}
double2Int# :: Double# -> Int#
double2Int# a1 = (GHC.Prim.double2Int#) a1
{-# NOINLINE double2Float# #-}
double2Float# :: Double# -> Float#
double2Float# a1 = (GHC.Prim.double2Float#) a1
{-# NOINLINE expDouble# #-}
expDouble# :: Double# -> Double#
expDouble# a1 = (GHC.Prim.expDouble#) a1
{-# NOINLINE logDouble# #-}
logDouble# :: Double# -> Double#
logDouble# a1 = (GHC.Prim.logDouble#) a1
{-# NOINLINE sqrtDouble# #-}
sqrtDouble# :: Double# -> Double#
sqrtDouble# a1 = (GHC.Prim.sqrtDouble#) a1
{-# NOINLINE sinDouble# #-}
sinDouble# :: Double# -> Double#
sinDouble# a1 = (GHC.Prim.sinDouble#) a1
{-# NOINLINE cosDouble# #-}
cosDouble# :: Double# -> Double#
cosDouble# a1 = (GHC.Prim.cosDouble#) a1
{-# NOINLINE tanDouble# #-}
tanDouble# :: Double# -> Double#
tanDouble# a1 = (GHC.Prim.tanDouble#) a1
{-# NOINLINE asinDouble# #-}
asinDouble# :: Double# -> Double#
asinDouble# a1 = (GHC.Prim.asinDouble#) a1
{-# NOINLINE acosDouble# #-}
acosDouble# :: Double# -> Double#
acosDouble# a1 = (GHC.Prim.acosDouble#) a1
{-# NOINLINE atanDouble# #-}
atanDouble# :: Double# -> Double#
atanDouble# a1 = (GHC.Prim.atanDouble#) a1
{-# NOINLINE sinhDouble# #-}
sinhDouble# :: Double# -> Double#
sinhDouble# a1 = (GHC.Prim.sinhDouble#) a1
{-# NOINLINE coshDouble# #-}
coshDouble# :: Double# -> Double#
coshDouble# a1 = (GHC.Prim.coshDouble#) a1
{-# NOINLINE tanhDouble# #-}
tanhDouble# :: Double# -> Double#
tanhDouble# a1 = (GHC.Prim.tanhDouble#) a1
{-# NOINLINE (**##) #-}
(**##) :: Double# -> Double# -> Double#
(**##) a1 a2 = (GHC.Prim.**##) a1 a2
{-# NOINLINE decodeDouble_2Int# #-}
decodeDouble_2Int# :: Double# -> (# Int#,Word#,Word#,Int# #)
decodeDouble_2Int# a1 = (GHC.Prim.decodeDouble_2Int#) a1
{-# NOINLINE gtFloat# #-}
gtFloat# :: Float# -> Float# -> Bool
gtFloat# a1 a2 = (GHC.Prim.gtFloat#) a1 a2
{-# NOINLINE geFloat# #-}
geFloat# :: Float# -> Float# -> Bool
geFloat# a1 a2 = (GHC.Prim.geFloat#) a1 a2
{-# NOINLINE eqFloat# #-}
eqFloat# :: Float# -> Float# -> Bool
eqFloat# a1 a2 = (GHC.Prim.eqFloat#) a1 a2
{-# NOINLINE neFloat# #-}
neFloat# :: Float# -> Float# -> Bool
neFloat# a1 a2 = (GHC.Prim.neFloat#) a1 a2
{-# NOINLINE ltFloat# #-}
ltFloat# :: Float# -> Float# -> Bool
ltFloat# a1 a2 = (GHC.Prim.ltFloat#) a1 a2
{-# NOINLINE leFloat# #-}
leFloat# :: Float# -> Float# -> Bool
leFloat# a1 a2 = (GHC.Prim.leFloat#) a1 a2
{-# NOINLINE plusFloat# #-}
plusFloat# :: Float# -> Float# -> Float#
plusFloat# a1 a2 = (GHC.Prim.plusFloat#) a1 a2
{-# NOINLINE minusFloat# #-}
minusFloat# :: Float# -> Float# -> Float#
minusFloat# a1 a2 = (GHC.Prim.minusFloat#) a1 a2
{-# NOINLINE timesFloat# #-}
timesFloat# :: Float# -> Float# -> Float#
timesFloat# a1 a2 = (GHC.Prim.timesFloat#) a1 a2
{-# NOINLINE divideFloat# #-}
divideFloat# :: Float# -> Float# -> Float#
divideFloat# a1 a2 = (GHC.Prim.divideFloat#) a1 a2
{-# NOINLINE negateFloat# #-}
negateFloat# :: Float# -> Float#
negateFloat# a1 = (GHC.Prim.negateFloat#) a1
{-# NOINLINE float2Int# #-}
float2Int# :: Float# -> Int#
float2Int# a1 = (GHC.Prim.float2Int#) a1
{-# NOINLINE expFloat# #-}
expFloat# :: Float# -> Float#
expFloat# a1 = (GHC.Prim.expFloat#) a1
{-# NOINLINE logFloat# #-}
logFloat# :: Float# -> Float#
logFloat# a1 = (GHC.Prim.logFloat#) a1
{-# NOINLINE sqrtFloat# #-}
sqrtFloat# :: Float# -> Float#
sqrtFloat# a1 = (GHC.Prim.sqrtFloat#) a1
{-# NOINLINE sinFloat# #-}
sinFloat# :: Float# -> Float#
sinFloat# a1 = (GHC.Prim.sinFloat#) a1
{-# NOINLINE cosFloat# #-}
cosFloat# :: Float# -> Float#
cosFloat# a1 = (GHC.Prim.cosFloat#) a1
{-# NOINLINE tanFloat# #-}
tanFloat# :: Float# -> Float#
tanFloat# a1 = (GHC.Prim.tanFloat#) a1
{-# NOINLINE asinFloat# #-}
asinFloat# :: Float# -> Float#
asinFloat# a1 = (GHC.Prim.asinFloat#) a1
{-# NOINLINE acosFloat# #-}
acosFloat# :: Float# -> Float#
acosFloat# a1 = (GHC.Prim.acosFloat#) a1
{-# NOINLINE atanFloat# #-}
atanFloat# :: Float# -> Float#
atanFloat# a1 = (GHC.Prim.atanFloat#) a1
{-# NOINLINE sinhFloat# #-}
sinhFloat# :: Float# -> Float#
sinhFloat# a1 = (GHC.Prim.sinhFloat#) a1
{-# NOINLINE coshFloat# #-}
coshFloat# :: Float# -> Float#
coshFloat# a1 = (GHC.Prim.coshFloat#) a1
{-# NOINLINE tanhFloat# #-}
tanhFloat# :: Float# -> Float#
tanhFloat# a1 = (GHC.Prim.tanhFloat#) a1
{-# NOINLINE powerFloat# #-}
powerFloat# :: Float# -> Float# -> Float#
powerFloat# a1 a2 = (GHC.Prim.powerFloat#) a1 a2
{-# NOINLINE float2Double# #-}
float2Double# :: Float# -> Double#
float2Double# a1 = (GHC.Prim.float2Double#) a1
{-# NOINLINE decodeFloat_Int# #-}
decodeFloat_Int# :: Float# -> (# Int#,Int# #)
decodeFloat_Int# a1 = (GHC.Prim.decodeFloat_Int#) a1
{-# NOINLINE newArray# #-}
newArray# :: Int# -> a -> State# s -> (# State# s,MutableArray# s a #)
newArray# a1 a2 a3 = (GHC.Prim.newArray#) a1 a2 a3
{-# NOINLINE sameMutableArray# #-}
sameMutableArray# :: MutableArray# s a -> MutableArray# s a -> Bool
sameMutableArray# a1 a2 = (GHC.Prim.sameMutableArray#) a1 a2
{-# NOINLINE readArray# #-}
readArray# :: MutableArray# s a -> Int# -> State# s -> (# State# s,a #)
readArray# a1 a2 a3 = (GHC.Prim.readArray#) a1 a2 a3
{-# NOINLINE writeArray# #-}
writeArray# :: MutableArray# s a -> Int# -> a -> State# s -> State# s
writeArray# a1 a2 a3 a4 = (GHC.Prim.writeArray#) a1 a2 a3 a4
{-# NOINLINE sizeofArray# #-}
sizeofArray# :: Array# a -> Int#
sizeofArray# a1 = (GHC.Prim.sizeofArray#) a1
{-# NOINLINE sizeofMutableArray# #-}
sizeofMutableArray# :: MutableArray# s a -> Int#
sizeofMutableArray# a1 = (GHC.Prim.sizeofMutableArray#) a1
{-# NOINLINE indexArray# #-}
indexArray# :: Array# a -> Int# -> (# a #)
indexArray# a1 a2 = (GHC.Prim.indexArray#) a1 a2
{-# NOINLINE unsafeFreezeArray# #-}
unsafeFreezeArray# :: MutableArray# s a -> State# s -> (# State# s,Array# a #)
unsafeFreezeArray# a1 a2 = (GHC.Prim.unsafeFreezeArray#) a1 a2
{-# NOINLINE unsafeThawArray# #-}
unsafeThawArray# :: Array# a -> State# s -> (# State# s,MutableArray# s a #)
unsafeThawArray# a1 a2 = (GHC.Prim.unsafeThawArray#) a1 a2
{-# NOINLINE copyArray# #-}
copyArray# :: Array# a -> Int# -> MutableArray# s a -> Int# -> Int# -> State# s -> State# s
copyArray# a1 a2 a3 a4 a5 a6 = (GHC.Prim.copyArray#) a1 a2 a3 a4 a5 a6
{-# NOINLINE copyMutableArray# #-}
copyMutableArray# :: MutableArray# s a -> Int# -> MutableArray# s a -> Int# -> Int# -> State# s -> State# s
copyMutableArray# a1 a2 a3 a4 a5 a6 = (GHC.Prim.copyMutableArray#) a1 a2 a3 a4 a5 a6
{-# NOINLINE cloneArray# #-}
cloneArray# :: Array# a -> Int# -> Int# -> Array# a
cloneArray# a1 a2 a3 = (GHC.Prim.cloneArray#) a1 a2 a3
{-# NOINLINE cloneMutableArray# #-}
cloneMutableArray# :: MutableArray# s a -> Int# -> Int# -> State# s -> (# State# s,MutableArray# s a #)
cloneMutableArray# a1 a2 a3 a4 = (GHC.Prim.cloneMutableArray#) a1 a2 a3 a4
{-# NOINLINE freezeArray# #-}
freezeArray# :: MutableArray# s a -> Int# -> Int# -> State# s -> (# State# s,Array# a #)
freezeArray# a1 a2 a3 a4 = (GHC.Prim.freezeArray#) a1 a2 a3 a4
{-# NOINLINE thawArray# #-}
thawArray# :: Array# a -> Int# -> Int# -> State# s -> (# State# s,MutableArray# s a #)
thawArray# a1 a2 a3 a4 = (GHC.Prim.thawArray#) a1 a2 a3 a4
{-# NOINLINE newByteArray# #-}
newByteArray# :: Int# -> State# s -> (# State# s,MutableByteArray# s #)
newByteArray# a1 a2 = (GHC.Prim.newByteArray#) a1 a2
{-# NOINLINE newPinnedByteArray# #-}
newPinnedByteArray# :: Int# -> State# s -> (# State# s,MutableByteArray# s #)
newPinnedByteArray# a1 a2 = (GHC.Prim.newPinnedByteArray#) a1 a2
{-# NOINLINE newAlignedPinnedByteArray# #-}
newAlignedPinnedByteArray# :: Int# -> Int# -> State# s -> (# State# s,MutableByteArray# s #)
newAlignedPinnedByteArray# a1 a2 a3 = (GHC.Prim.newAlignedPinnedByteArray#) a1 a2 a3
{-# NOINLINE byteArrayContents# #-}
byteArrayContents# :: ByteArray# -> Addr#
byteArrayContents# a1 = (GHC.Prim.byteArrayContents#) a1
{-# NOINLINE sameMutableByteArray# #-}
sameMutableByteArray# :: MutableByteArray# s -> MutableByteArray# s -> Bool
sameMutableByteArray# a1 a2 = (GHC.Prim.sameMutableByteArray#) a1 a2
{-# NOINLINE unsafeFreezeByteArray# #-}
unsafeFreezeByteArray# :: MutableByteArray# s -> State# s -> (# State# s,ByteArray# #)
unsafeFreezeByteArray# a1 a2 = (GHC.Prim.unsafeFreezeByteArray#) a1 a2
{-# NOINLINE sizeofByteArray# #-}
sizeofByteArray# :: ByteArray# -> Int#
sizeofByteArray# a1 = (GHC.Prim.sizeofByteArray#) a1
{-# NOINLINE sizeofMutableByteArray# #-}
sizeofMutableByteArray# :: MutableByteArray# s -> Int#
sizeofMutableByteArray# a1 = (GHC.Prim.sizeofMutableByteArray#) a1
{-# NOINLINE indexCharArray# #-}
indexCharArray# :: ByteArray# -> Int# -> Char#
indexCharArray# a1 a2 = (GHC.Prim.indexCharArray#) a1 a2
{-# NOINLINE indexWideCharArray# #-}
indexWideCharArray# :: ByteArray# -> Int# -> Char#
indexWideCharArray# a1 a2 = (GHC.Prim.indexWideCharArray#) a1 a2
{-# NOINLINE indexIntArray# #-}
indexIntArray# :: ByteArray# -> Int# -> Int#
indexIntArray# a1 a2 = (GHC.Prim.indexIntArray#) a1 a2
{-# NOINLINE indexWordArray# #-}
indexWordArray# :: ByteArray# -> Int# -> Word#
indexWordArray# a1 a2 = (GHC.Prim.indexWordArray#) a1 a2
{-# NOINLINE indexAddrArray# #-}
indexAddrArray# :: ByteArray# -> Int# -> Addr#
indexAddrArray# a1 a2 = (GHC.Prim.indexAddrArray#) a1 a2
{-# NOINLINE indexFloatArray# #-}
indexFloatArray# :: ByteArray# -> Int# -> Float#
indexFloatArray# a1 a2 = (GHC.Prim.indexFloatArray#) a1 a2
{-# NOINLINE indexDoubleArray# #-}
indexDoubleArray# :: ByteArray# -> Int# -> Double#
indexDoubleArray# a1 a2 = (GHC.Prim.indexDoubleArray#) a1 a2
{-# NOINLINE indexStablePtrArray# #-}
indexStablePtrArray# :: ByteArray# -> Int# -> StablePtr# a
indexStablePtrArray# a1 a2 = (GHC.Prim.indexStablePtrArray#) a1 a2
{-# NOINLINE indexInt8Array# #-}
indexInt8Array# :: ByteArray# -> Int# -> Int#
indexInt8Array# a1 a2 = (GHC.Prim.indexInt8Array#) a1 a2
{-# NOINLINE indexInt16Array# #-}
indexInt16Array# :: ByteArray# -> Int# -> Int#
indexInt16Array# a1 a2 = (GHC.Prim.indexInt16Array#) a1 a2
{-# NOINLINE indexInt32Array# #-}
indexInt32Array# :: ByteArray# -> Int# -> Int#
indexInt32Array# a1 a2 = (GHC.Prim.indexInt32Array#) a1 a2
{-# NOINLINE indexInt64Array# #-}
indexInt64Array# :: ByteArray# -> Int# -> Int#
indexInt64Array# a1 a2 = (GHC.Prim.indexInt64Array#) a1 a2
{-# NOINLINE indexWord8Array# #-}
indexWord8Array# :: ByteArray# -> Int# -> Word#
indexWord8Array# a1 a2 = (GHC.Prim.indexWord8Array#) a1 a2
{-# NOINLINE indexWord16Array# #-}
indexWord16Array# :: ByteArray# -> Int# -> Word#
indexWord16Array# a1 a2 = (GHC.Prim.indexWord16Array#) a1 a2
{-# NOINLINE indexWord32Array# #-}
indexWord32Array# :: ByteArray# -> Int# -> Word#
indexWord32Array# a1 a2 = (GHC.Prim.indexWord32Array#) a1 a2
{-# NOINLINE indexWord64Array# #-}
indexWord64Array# :: ByteArray# -> Int# -> Word#
indexWord64Array# a1 a2 = (GHC.Prim.indexWord64Array#) a1 a2
{-# NOINLINE readCharArray# #-}
readCharArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Char# #)
readCharArray# a1 a2 a3 = (GHC.Prim.readCharArray#) a1 a2 a3
{-# NOINLINE readWideCharArray# #-}
readWideCharArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Char# #)
readWideCharArray# a1 a2 a3 = (GHC.Prim.readWideCharArray#) a1 a2 a3
{-# NOINLINE readIntArray# #-}
readIntArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Int# #)
readIntArray# a1 a2 a3 = (GHC.Prim.readIntArray#) a1 a2 a3
{-# NOINLINE readWordArray# #-}
readWordArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Word# #)
readWordArray# a1 a2 a3 = (GHC.Prim.readWordArray#) a1 a2 a3
{-# NOINLINE readAddrArray# #-}
readAddrArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Addr# #)
readAddrArray# a1 a2 a3 = (GHC.Prim.readAddrArray#) a1 a2 a3
{-# NOINLINE readFloatArray# #-}
readFloatArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Float# #)
readFloatArray# a1 a2 a3 = (GHC.Prim.readFloatArray#) a1 a2 a3
{-# NOINLINE readDoubleArray# #-}
readDoubleArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Double# #)
readDoubleArray# a1 a2 a3 = (GHC.Prim.readDoubleArray#) a1 a2 a3
{-# NOINLINE readStablePtrArray# #-}
readStablePtrArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,StablePtr# a #)
readStablePtrArray# a1 a2 a3 = (GHC.Prim.readStablePtrArray#) a1 a2 a3
{-# NOINLINE readInt8Array# #-}
readInt8Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Int# #)
readInt8Array# a1 a2 a3 = (GHC.Prim.readInt8Array#) a1 a2 a3
{-# NOINLINE readInt16Array# #-}
readInt16Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Int# #)
readInt16Array# a1 a2 a3 = (GHC.Prim.readInt16Array#) a1 a2 a3
{-# NOINLINE readInt32Array# #-}
readInt32Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Int# #)
readInt32Array# a1 a2 a3 = (GHC.Prim.readInt32Array#) a1 a2 a3
{-# NOINLINE readInt64Array# #-}
readInt64Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Int# #)
readInt64Array# a1 a2 a3 = (GHC.Prim.readInt64Array#) a1 a2 a3
{-# NOINLINE readWord8Array# #-}
readWord8Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Word# #)
readWord8Array# a1 a2 a3 = (GHC.Prim.readWord8Array#) a1 a2 a3
{-# NOINLINE readWord16Array# #-}
readWord16Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Word# #)
readWord16Array# a1 a2 a3 = (GHC.Prim.readWord16Array#) a1 a2 a3
{-# NOINLINE readWord32Array# #-}
readWord32Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Word# #)
readWord32Array# a1 a2 a3 = (GHC.Prim.readWord32Array#) a1 a2 a3
{-# NOINLINE readWord64Array# #-}
readWord64Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Word# #)
readWord64Array# a1 a2 a3 = (GHC.Prim.readWord64Array#) a1 a2 a3
{-# NOINLINE writeCharArray# #-}
writeCharArray# :: MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
writeCharArray# a1 a2 a3 a4 = (GHC.Prim.writeCharArray#) a1 a2 a3 a4
{-# NOINLINE writeWideCharArray# #-}
writeWideCharArray# :: MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
writeWideCharArray# a1 a2 a3 a4 = (GHC.Prim.writeWideCharArray#) a1 a2 a3 a4
{-# NOINLINE writeIntArray# #-}
writeIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
writeIntArray# a1 a2 a3 a4 = (GHC.Prim.writeIntArray#) a1 a2 a3 a4
{-# NOINLINE writeWordArray# #-}
writeWordArray# :: MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
writeWordArray# a1 a2 a3 a4 = (GHC.Prim.writeWordArray#) a1 a2 a3 a4
{-# NOINLINE writeAddrArray# #-}
writeAddrArray# :: MutableByteArray# s -> Int# -> Addr# -> State# s -> State# s
writeAddrArray# a1 a2 a3 a4 = (GHC.Prim.writeAddrArray#) a1 a2 a3 a4
{-# NOINLINE writeFloatArray# #-}
writeFloatArray# :: MutableByteArray# s -> Int# -> Float# -> State# s -> State# s
writeFloatArray# a1 a2 a3 a4 = (GHC.Prim.writeFloatArray#) a1 a2 a3 a4
{-# NOINLINE writeDoubleArray# #-}
writeDoubleArray# :: MutableByteArray# s -> Int# -> Double# -> State# s -> State# s
writeDoubleArray# a1 a2 a3 a4 = (GHC.Prim.writeDoubleArray#) a1 a2 a3 a4
{-# NOINLINE writeStablePtrArray# #-}
writeStablePtrArray# :: MutableByteArray# s -> Int# -> StablePtr# a -> State# s -> State# s
writeStablePtrArray# a1 a2 a3 a4 = (GHC.Prim.writeStablePtrArray#) a1 a2 a3 a4
{-# NOINLINE writeInt8Array# #-}
writeInt8Array# :: MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
writeInt8Array# a1 a2 a3 a4 = (GHC.Prim.writeInt8Array#) a1 a2 a3 a4
{-# NOINLINE writeInt16Array# #-}
writeInt16Array# :: MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
writeInt16Array# a1 a2 a3 a4 = (GHC.Prim.writeInt16Array#) a1 a2 a3 a4
{-# NOINLINE writeInt32Array# #-}
writeInt32Array# :: MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
writeInt32Array# a1 a2 a3 a4 = (GHC.Prim.writeInt32Array#) a1 a2 a3 a4
{-# NOINLINE writeInt64Array# #-}
writeInt64Array# :: MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
writeInt64Array# a1 a2 a3 a4 = (GHC.Prim.writeInt64Array#) a1 a2 a3 a4
{-# NOINLINE writeWord8Array# #-}
writeWord8Array# :: MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
writeWord8Array# a1 a2 a3 a4 = (GHC.Prim.writeWord8Array#) a1 a2 a3 a4
{-# NOINLINE writeWord16Array# #-}
writeWord16Array# :: MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
writeWord16Array# a1 a2 a3 a4 = (GHC.Prim.writeWord16Array#) a1 a2 a3 a4
{-# NOINLINE writeWord32Array# #-}
writeWord32Array# :: MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
writeWord32Array# a1 a2 a3 a4 = (GHC.Prim.writeWord32Array#) a1 a2 a3 a4
{-# NOINLINE writeWord64Array# #-}
writeWord64Array# :: MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
writeWord64Array# a1 a2 a3 a4 = (GHC.Prim.writeWord64Array#) a1 a2 a3 a4
{-# NOINLINE copyByteArray# #-}
copyByteArray# :: ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyByteArray# a1 a2 a3 a4 a5 a6 = (GHC.Prim.copyByteArray#) a1 a2 a3 a4 a5 a6
{-# NOINLINE copyMutableByteArray# #-}
copyMutableByteArray# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyMutableByteArray# a1 a2 a3 a4 a5 a6 = (GHC.Prim.copyMutableByteArray#) a1 a2 a3 a4 a5 a6
{-# NOINLINE setByteArray# #-}
setByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int# -> State# s -> State# s
setByteArray# a1 a2 a3 a4 a5 = (GHC.Prim.setByteArray#) a1 a2 a3 a4 a5
{-# NOINLINE newArrayArray# #-}
newArrayArray# :: Int# -> State# s -> (# State# s,MutableArrayArray# s #)
newArrayArray# a1 a2 = (GHC.Prim.newArrayArray#) a1 a2
{-# NOINLINE sameMutableArrayArray# #-}
sameMutableArrayArray# :: MutableArrayArray# s -> MutableArrayArray# s -> Bool
sameMutableArrayArray# a1 a2 = (GHC.Prim.sameMutableArrayArray#) a1 a2
{-# NOINLINE unsafeFreezeArrayArray# #-}
unsafeFreezeArrayArray# :: MutableArrayArray# s -> State# s -> (# State# s,ArrayArray# #)
unsafeFreezeArrayArray# a1 a2 = (GHC.Prim.unsafeFreezeArrayArray#) a1 a2
{-# NOINLINE sizeofArrayArray# #-}
sizeofArrayArray# :: ArrayArray# -> Int#
sizeofArrayArray# a1 = (GHC.Prim.sizeofArrayArray#) a1
{-# NOINLINE sizeofMutableArrayArray# #-}
sizeofMutableArrayArray# :: MutableArrayArray# s -> Int#
sizeofMutableArrayArray# a1 = (GHC.Prim.sizeofMutableArrayArray#) a1
{-# NOINLINE indexByteArrayArray# #-}
indexByteArrayArray# :: ArrayArray# -> Int# -> ByteArray#
indexByteArrayArray# a1 a2 = (GHC.Prim.indexByteArrayArray#) a1 a2
{-# NOINLINE indexArrayArrayArray# #-}
indexArrayArrayArray# :: ArrayArray# -> Int# -> ArrayArray#
indexArrayArrayArray# a1 a2 = (GHC.Prim.indexArrayArrayArray#) a1 a2
{-# NOINLINE readByteArrayArray# #-}
readByteArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s,ByteArray# #)
readByteArrayArray# a1 a2 a3 = (GHC.Prim.readByteArrayArray#) a1 a2 a3
{-# NOINLINE readMutableByteArrayArray# #-}
readMutableByteArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s,MutableByteArray# s #)
readMutableByteArrayArray# a1 a2 a3 = (GHC.Prim.readMutableByteArrayArray#) a1 a2 a3
{-# NOINLINE readArrayArrayArray# #-}
readArrayArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s,ArrayArray# #)
readArrayArrayArray# a1 a2 a3 = (GHC.Prim.readArrayArrayArray#) a1 a2 a3
{-# NOINLINE readMutableArrayArrayArray# #-}
readMutableArrayArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s,MutableArrayArray# s #)
readMutableArrayArrayArray# a1 a2 a3 = (GHC.Prim.readMutableArrayArrayArray#) a1 a2 a3
{-# NOINLINE writeByteArrayArray# #-}
writeByteArrayArray# :: MutableArrayArray# s -> Int# -> ByteArray# -> State# s -> State# s
writeByteArrayArray# a1 a2 a3 a4 = (GHC.Prim.writeByteArrayArray#) a1 a2 a3 a4
{-# NOINLINE writeMutableByteArrayArray# #-}
writeMutableByteArrayArray# :: MutableArrayArray# s -> Int# -> MutableByteArray# s -> State# s -> State# s
writeMutableByteArrayArray# a1 a2 a3 a4 = (GHC.Prim.writeMutableByteArrayArray#) a1 a2 a3 a4
{-# NOINLINE writeArrayArrayArray# #-}
writeArrayArrayArray# :: MutableArrayArray# s -> Int# -> ArrayArray# -> State# s -> State# s
writeArrayArrayArray# a1 a2 a3 a4 = (GHC.Prim.writeArrayArrayArray#) a1 a2 a3 a4
{-# NOINLINE writeMutableArrayArrayArray# #-}
writeMutableArrayArrayArray# :: MutableArrayArray# s -> Int# -> MutableArrayArray# s -> State# s -> State# s
writeMutableArrayArrayArray# a1 a2 a3 a4 = (GHC.Prim.writeMutableArrayArrayArray#) a1 a2 a3 a4
{-# NOINLINE copyArrayArray# #-}
copyArrayArray# :: ArrayArray# -> Int# -> MutableArrayArray# s -> Int# -> Int# -> State# s -> State# s
copyArrayArray# a1 a2 a3 a4 a5 a6 = (GHC.Prim.copyArrayArray#) a1 a2 a3 a4 a5 a6
{-# NOINLINE copyMutableArrayArray# #-}
copyMutableArrayArray# :: MutableArrayArray# s -> Int# -> MutableArrayArray# s -> Int# -> Int# -> State# s -> State# s
copyMutableArrayArray# a1 a2 a3 a4 a5 a6 = (GHC.Prim.copyMutableArrayArray#) a1 a2 a3 a4 a5 a6
{-# NOINLINE plusAddr# #-}
plusAddr# :: Addr# -> Int# -> Addr#
plusAddr# a1 a2 = (GHC.Prim.plusAddr#) a1 a2
{-# NOINLINE minusAddr# #-}
minusAddr# :: Addr# -> Addr# -> Int#
minusAddr# a1 a2 = (GHC.Prim.minusAddr#) a1 a2
{-# NOINLINE remAddr# #-}
remAddr# :: Addr# -> Int# -> Int#
remAddr# a1 a2 = (GHC.Prim.remAddr#) a1 a2
{-# NOINLINE addr2Int# #-}
addr2Int# :: Addr# -> Int#
addr2Int# a1 = (GHC.Prim.addr2Int#) a1
{-# NOINLINE int2Addr# #-}
int2Addr# :: Int# -> Addr#
int2Addr# a1 = (GHC.Prim.int2Addr#) a1
{-# NOINLINE gtAddr# #-}
gtAddr# :: Addr# -> Addr# -> Bool
gtAddr# a1 a2 = (GHC.Prim.gtAddr#) a1 a2
{-# NOINLINE geAddr# #-}
geAddr# :: Addr# -> Addr# -> Bool
geAddr# a1 a2 = (GHC.Prim.geAddr#) a1 a2
{-# NOINLINE eqAddr# #-}
eqAddr# :: Addr# -> Addr# -> Bool
eqAddr# a1 a2 = (GHC.Prim.eqAddr#) a1 a2
{-# NOINLINE neAddr# #-}
neAddr# :: Addr# -> Addr# -> Bool
neAddr# a1 a2 = (GHC.Prim.neAddr#) a1 a2
{-# NOINLINE ltAddr# #-}
ltAddr# :: Addr# -> Addr# -> Bool
ltAddr# a1 a2 = (GHC.Prim.ltAddr#) a1 a2
{-# NOINLINE leAddr# #-}
leAddr# :: Addr# -> Addr# -> Bool
leAddr# a1 a2 = (GHC.Prim.leAddr#) a1 a2
{-# NOINLINE indexCharOffAddr# #-}
indexCharOffAddr# :: Addr# -> Int# -> Char#
indexCharOffAddr# a1 a2 = (GHC.Prim.indexCharOffAddr#) a1 a2
{-# NOINLINE indexWideCharOffAddr# #-}
indexWideCharOffAddr# :: Addr# -> Int# -> Char#
indexWideCharOffAddr# a1 a2 = (GHC.Prim.indexWideCharOffAddr#) a1 a2
{-# NOINLINE indexIntOffAddr# #-}
indexIntOffAddr# :: Addr# -> Int# -> Int#
indexIntOffAddr# a1 a2 = (GHC.Prim.indexIntOffAddr#) a1 a2
{-# NOINLINE indexWordOffAddr# #-}
indexWordOffAddr# :: Addr# -> Int# -> Word#
indexWordOffAddr# a1 a2 = (GHC.Prim.indexWordOffAddr#) a1 a2
{-# NOINLINE indexAddrOffAddr# #-}
indexAddrOffAddr# :: Addr# -> Int# -> Addr#
indexAddrOffAddr# a1 a2 = (GHC.Prim.indexAddrOffAddr#) a1 a2
{-# NOINLINE indexFloatOffAddr# #-}
indexFloatOffAddr# :: Addr# -> Int# -> Float#
indexFloatOffAddr# a1 a2 = (GHC.Prim.indexFloatOffAddr#) a1 a2
{-# NOINLINE indexDoubleOffAddr# #-}
indexDoubleOffAddr# :: Addr# -> Int# -> Double#
indexDoubleOffAddr# a1 a2 = (GHC.Prim.indexDoubleOffAddr#) a1 a2
{-# NOINLINE indexStablePtrOffAddr# #-}
indexStablePtrOffAddr# :: Addr# -> Int# -> StablePtr# a
indexStablePtrOffAddr# a1 a2 = (GHC.Prim.indexStablePtrOffAddr#) a1 a2
{-# NOINLINE indexInt8OffAddr# #-}
indexInt8OffAddr# :: Addr# -> Int# -> Int#
indexInt8OffAddr# a1 a2 = (GHC.Prim.indexInt8OffAddr#) a1 a2
{-# NOINLINE indexInt16OffAddr# #-}
indexInt16OffAddr# :: Addr# -> Int# -> Int#
indexInt16OffAddr# a1 a2 = (GHC.Prim.indexInt16OffAddr#) a1 a2
{-# NOINLINE indexInt32OffAddr# #-}
indexInt32OffAddr# :: Addr# -> Int# -> Int#
indexInt32OffAddr# a1 a2 = (GHC.Prim.indexInt32OffAddr#) a1 a2
{-# NOINLINE indexInt64OffAddr# #-}
indexInt64OffAddr# :: Addr# -> Int# -> Int#
indexInt64OffAddr# a1 a2 = (GHC.Prim.indexInt64OffAddr#) a1 a2
{-# NOINLINE indexWord8OffAddr# #-}
indexWord8OffAddr# :: Addr# -> Int# -> Word#
indexWord8OffAddr# a1 a2 = (GHC.Prim.indexWord8OffAddr#) a1 a2
{-# NOINLINE indexWord16OffAddr# #-}
indexWord16OffAddr# :: Addr# -> Int# -> Word#
indexWord16OffAddr# a1 a2 = (GHC.Prim.indexWord16OffAddr#) a1 a2
{-# NOINLINE indexWord32OffAddr# #-}
indexWord32OffAddr# :: Addr# -> Int# -> Word#
indexWord32OffAddr# a1 a2 = (GHC.Prim.indexWord32OffAddr#) a1 a2
{-# NOINLINE indexWord64OffAddr# #-}
indexWord64OffAddr# :: Addr# -> Int# -> Word#
indexWord64OffAddr# a1 a2 = (GHC.Prim.indexWord64OffAddr#) a1 a2
{-# NOINLINE readCharOffAddr# #-}
readCharOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Char# #)
readCharOffAddr# a1 a2 a3 = (GHC.Prim.readCharOffAddr#) a1 a2 a3
{-# NOINLINE readWideCharOffAddr# #-}
readWideCharOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Char# #)
readWideCharOffAddr# a1 a2 a3 = (GHC.Prim.readWideCharOffAddr#) a1 a2 a3
{-# NOINLINE readIntOffAddr# #-}
readIntOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Int# #)
readIntOffAddr# a1 a2 a3 = (GHC.Prim.readIntOffAddr#) a1 a2 a3
{-# NOINLINE readWordOffAddr# #-}
readWordOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Word# #)
readWordOffAddr# a1 a2 a3 = (GHC.Prim.readWordOffAddr#) a1 a2 a3
{-# NOINLINE readAddrOffAddr# #-}
readAddrOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Addr# #)
readAddrOffAddr# a1 a2 a3 = (GHC.Prim.readAddrOffAddr#) a1 a2 a3
{-# NOINLINE readFloatOffAddr# #-}
readFloatOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Float# #)
readFloatOffAddr# a1 a2 a3 = (GHC.Prim.readFloatOffAddr#) a1 a2 a3
{-# NOINLINE readDoubleOffAddr# #-}
readDoubleOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Double# #)
readDoubleOffAddr# a1 a2 a3 = (GHC.Prim.readDoubleOffAddr#) a1 a2 a3
{-# NOINLINE readStablePtrOffAddr# #-}
readStablePtrOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,StablePtr# a #)
readStablePtrOffAddr# a1 a2 a3 = (GHC.Prim.readStablePtrOffAddr#) a1 a2 a3
{-# NOINLINE readInt8OffAddr# #-}
readInt8OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Int# #)
readInt8OffAddr# a1 a2 a3 = (GHC.Prim.readInt8OffAddr#) a1 a2 a3
{-# NOINLINE readInt16OffAddr# #-}
readInt16OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Int# #)
readInt16OffAddr# a1 a2 a3 = (GHC.Prim.readInt16OffAddr#) a1 a2 a3
{-# NOINLINE readInt32OffAddr# #-}
readInt32OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Int# #)
readInt32OffAddr# a1 a2 a3 = (GHC.Prim.readInt32OffAddr#) a1 a2 a3
{-# NOINLINE readInt64OffAddr# #-}
readInt64OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Int# #)
readInt64OffAddr# a1 a2 a3 = (GHC.Prim.readInt64OffAddr#) a1 a2 a3
{-# NOINLINE readWord8OffAddr# #-}
readWord8OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Word# #)
readWord8OffAddr# a1 a2 a3 = (GHC.Prim.readWord8OffAddr#) a1 a2 a3
{-# NOINLINE readWord16OffAddr# #-}
readWord16OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Word# #)
readWord16OffAddr# a1 a2 a3 = (GHC.Prim.readWord16OffAddr#) a1 a2 a3
{-# NOINLINE readWord32OffAddr# #-}
readWord32OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Word# #)
readWord32OffAddr# a1 a2 a3 = (GHC.Prim.readWord32OffAddr#) a1 a2 a3
{-# NOINLINE readWord64OffAddr# #-}
readWord64OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Word# #)
readWord64OffAddr# a1 a2 a3 = (GHC.Prim.readWord64OffAddr#) a1 a2 a3
{-# NOINLINE writeCharOffAddr# #-}
writeCharOffAddr# :: Addr# -> Int# -> Char# -> State# s -> State# s
writeCharOffAddr# a1 a2 a3 a4 = (GHC.Prim.writeCharOffAddr#) a1 a2 a3 a4
{-# NOINLINE writeWideCharOffAddr# #-}
writeWideCharOffAddr# :: Addr# -> Int# -> Char# -> State# s -> State# s
writeWideCharOffAddr# a1 a2 a3 a4 = (GHC.Prim.writeWideCharOffAddr#) a1 a2 a3 a4
{-# NOINLINE writeIntOffAddr# #-}
writeIntOffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
writeIntOffAddr# a1 a2 a3 a4 = (GHC.Prim.writeIntOffAddr#) a1 a2 a3 a4
{-# NOINLINE writeWordOffAddr# #-}
writeWordOffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
writeWordOffAddr# a1 a2 a3 a4 = (GHC.Prim.writeWordOffAddr#) a1 a2 a3 a4
{-# NOINLINE writeAddrOffAddr# #-}
writeAddrOffAddr# :: Addr# -> Int# -> Addr# -> State# s -> State# s
writeAddrOffAddr# a1 a2 a3 a4 = (GHC.Prim.writeAddrOffAddr#) a1 a2 a3 a4
{-# NOINLINE writeFloatOffAddr# #-}
writeFloatOffAddr# :: Addr# -> Int# -> Float# -> State# s -> State# s
writeFloatOffAddr# a1 a2 a3 a4 = (GHC.Prim.writeFloatOffAddr#) a1 a2 a3 a4
{-# NOINLINE writeDoubleOffAddr# #-}
writeDoubleOffAddr# :: Addr# -> Int# -> Double# -> State# s -> State# s
writeDoubleOffAddr# a1 a2 a3 a4 = (GHC.Prim.writeDoubleOffAddr#) a1 a2 a3 a4
{-# NOINLINE writeStablePtrOffAddr# #-}
writeStablePtrOffAddr# :: Addr# -> Int# -> StablePtr# a -> State# s -> State# s
writeStablePtrOffAddr# a1 a2 a3 a4 = (GHC.Prim.writeStablePtrOffAddr#) a1 a2 a3 a4
{-# NOINLINE writeInt8OffAddr# #-}
writeInt8OffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
writeInt8OffAddr# a1 a2 a3 a4 = (GHC.Prim.writeInt8OffAddr#) a1 a2 a3 a4
{-# NOINLINE writeInt16OffAddr# #-}
writeInt16OffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
writeInt16OffAddr# a1 a2 a3 a4 = (GHC.Prim.writeInt16OffAddr#) a1 a2 a3 a4
{-# NOINLINE writeInt32OffAddr# #-}
writeInt32OffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
writeInt32OffAddr# a1 a2 a3 a4 = (GHC.Prim.writeInt32OffAddr#) a1 a2 a3 a4
{-# NOINLINE writeInt64OffAddr# #-}
writeInt64OffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
writeInt64OffAddr# a1 a2 a3 a4 = (GHC.Prim.writeInt64OffAddr#) a1 a2 a3 a4
{-# NOINLINE writeWord8OffAddr# #-}
writeWord8OffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
writeWord8OffAddr# a1 a2 a3 a4 = (GHC.Prim.writeWord8OffAddr#) a1 a2 a3 a4
{-# NOINLINE writeWord16OffAddr# #-}
writeWord16OffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
writeWord16OffAddr# a1 a2 a3 a4 = (GHC.Prim.writeWord16OffAddr#) a1 a2 a3 a4
{-# NOINLINE writeWord32OffAddr# #-}
writeWord32OffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
writeWord32OffAddr# a1 a2 a3 a4 = (GHC.Prim.writeWord32OffAddr#) a1 a2 a3 a4
{-# NOINLINE writeWord64OffAddr# #-}
writeWord64OffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
writeWord64OffAddr# a1 a2 a3 a4 = (GHC.Prim.writeWord64OffAddr#) a1 a2 a3 a4
{-# NOINLINE newMutVar# #-}
newMutVar# :: a -> State# s -> (# State# s,MutVar# s a #)
newMutVar# a1 a2 = (GHC.Prim.newMutVar#) a1 a2
{-# NOINLINE readMutVar# #-}
readMutVar# :: MutVar# s a -> State# s -> (# State# s,a #)
readMutVar# a1 a2 = (GHC.Prim.readMutVar#) a1 a2
{-# NOINLINE writeMutVar# #-}
writeMutVar# :: MutVar# s a -> a -> State# s -> State# s
writeMutVar# a1 a2 a3 = (GHC.Prim.writeMutVar#) a1 a2 a3
{-# NOINLINE sameMutVar# #-}
sameMutVar# :: MutVar# s a -> MutVar# s a -> Bool
sameMutVar# a1 a2 = (GHC.Prim.sameMutVar#) a1 a2
{-# NOINLINE atomicModifyMutVar# #-}
atomicModifyMutVar# :: MutVar# s a -> (a -> b) -> State# s -> (# State# s,c #)
atomicModifyMutVar# a1 a2 a3 = (GHC.Prim.atomicModifyMutVar#) a1 a2 a3
{-# NOINLINE casMutVar# #-}
casMutVar# :: MutVar# s a -> a -> a -> State# s -> (# State# s,Int#,a #)
casMutVar# a1 a2 a3 a4 = (GHC.Prim.casMutVar#) a1 a2 a3 a4
{-# NOINLINE catch# #-}
catch# :: (State# (RealWorld) -> (# State# (RealWorld),a #)) -> (b -> State# (RealWorld) -> (# State# (RealWorld),a #)) -> State# (RealWorld) -> (# State# (RealWorld),a #)
catch# a1 a2 a3 = (GHC.Prim.catch#) a1 a2 a3
{-# NOINLINE raise# #-}
raise# :: a -> b
raise# a1 = (GHC.Prim.raise#) a1
{-# NOINLINE raiseIO# #-}
raiseIO# :: a -> State# (RealWorld) -> (# State# (RealWorld),b #)
raiseIO# a1 a2 = (GHC.Prim.raiseIO#) a1 a2
{-# NOINLINE maskAsyncExceptions# #-}
maskAsyncExceptions# :: (State# (RealWorld) -> (# State# (RealWorld),a #)) -> State# (RealWorld) -> (# State# (RealWorld),a #)
maskAsyncExceptions# a1 a2 = (GHC.Prim.maskAsyncExceptions#) a1 a2
{-# NOINLINE maskUninterruptible# #-}
maskUninterruptible# :: (State# (RealWorld) -> (# State# (RealWorld),a #)) -> State# (RealWorld) -> (# State# (RealWorld),a #)
maskUninterruptible# a1 a2 = (GHC.Prim.maskUninterruptible#) a1 a2
{-# NOINLINE unmaskAsyncExceptions# #-}
unmaskAsyncExceptions# :: (State# (RealWorld) -> (# State# (RealWorld),a #)) -> State# (RealWorld) -> (# State# (RealWorld),a #)
unmaskAsyncExceptions# a1 a2 = (GHC.Prim.unmaskAsyncExceptions#) a1 a2
{-# NOINLINE getMaskingState# #-}
getMaskingState# :: State# (RealWorld) -> (# State# (RealWorld),Int# #)
getMaskingState# a1 = (GHC.Prim.getMaskingState#) a1
{-# NOINLINE atomically# #-}
atomically# :: (State# (RealWorld) -> (# State# (RealWorld),a #)) -> State# (RealWorld) -> (# State# (RealWorld),a #)
atomically# a1 a2 = (GHC.Prim.atomically#) a1 a2
{-# NOINLINE retry# #-}
retry# :: State# (RealWorld) -> (# State# (RealWorld),a #)
retry# a1 = (GHC.Prim.retry#) a1
{-# NOINLINE catchRetry# #-}
catchRetry# :: (State# (RealWorld) -> (# State# (RealWorld),a #)) -> (State# (RealWorld) -> (# State# (RealWorld),a #)) -> State# (RealWorld) -> (# State# (RealWorld),a #)
catchRetry# a1 a2 a3 = (GHC.Prim.catchRetry#) a1 a2 a3
{-# NOINLINE catchSTM# #-}
catchSTM# :: (State# (RealWorld) -> (# State# (RealWorld),a #)) -> (b -> State# (RealWorld) -> (# State# (RealWorld),a #)) -> State# (RealWorld) -> (# State# (RealWorld),a #)
catchSTM# a1 a2 a3 = (GHC.Prim.catchSTM#) a1 a2 a3
{-# NOINLINE check# #-}
check# :: (State# (RealWorld) -> (# State# (RealWorld),a #)) -> State# (RealWorld) -> (# State# (RealWorld),() #)
check# a1 a2 = (GHC.Prim.check#) a1 a2
{-# NOINLINE newTVar# #-}
newTVar# :: a -> State# s -> (# State# s,TVar# s a #)
newTVar# a1 a2 = (GHC.Prim.newTVar#) a1 a2
{-# NOINLINE readTVar# #-}
readTVar# :: TVar# s a -> State# s -> (# State# s,a #)
readTVar# a1 a2 = (GHC.Prim.readTVar#) a1 a2
{-# NOINLINE readTVarIO# #-}
readTVarIO# :: TVar# s a -> State# s -> (# State# s,a #)
readTVarIO# a1 a2 = (GHC.Prim.readTVarIO#) a1 a2
{-# NOINLINE writeTVar# #-}
writeTVar# :: TVar# s a -> a -> State# s -> State# s
writeTVar# a1 a2 a3 = (GHC.Prim.writeTVar#) a1 a2 a3
{-# NOINLINE sameTVar# #-}
sameTVar# :: TVar# s a -> TVar# s a -> Bool
sameTVar# a1 a2 = (GHC.Prim.sameTVar#) a1 a2
{-# NOINLINE newMVar# #-}
newMVar# :: State# s -> (# State# s,MVar# s a #)
newMVar# a1 = (GHC.Prim.newMVar#) a1
{-# NOINLINE takeMVar# #-}
takeMVar# :: MVar# s a -> State# s -> (# State# s,a #)
takeMVar# a1 a2 = (GHC.Prim.takeMVar#) a1 a2
{-# NOINLINE tryTakeMVar# #-}
tryTakeMVar# :: MVar# s a -> State# s -> (# State# s,Int#,a #)
tryTakeMVar# a1 a2 = (GHC.Prim.tryTakeMVar#) a1 a2
{-# NOINLINE putMVar# #-}
putMVar# :: MVar# s a -> a -> State# s -> State# s
putMVar# a1 a2 a3 = (GHC.Prim.putMVar#) a1 a2 a3
{-# NOINLINE tryPutMVar# #-}
tryPutMVar# :: MVar# s a -> a -> State# s -> (# State# s,Int# #)
tryPutMVar# a1 a2 a3 = (GHC.Prim.tryPutMVar#) a1 a2 a3
{-# NOINLINE sameMVar# #-}
sameMVar# :: MVar# s a -> MVar# s a -> Bool
sameMVar# a1 a2 = (GHC.Prim.sameMVar#) a1 a2
{-# NOINLINE isEmptyMVar# #-}
isEmptyMVar# :: MVar# s a -> State# s -> (# State# s,Int# #)
isEmptyMVar# a1 a2 = (GHC.Prim.isEmptyMVar#) a1 a2
{-# NOINLINE delay# #-}
delay# :: Int# -> State# s -> State# s
delay# a1 a2 = (GHC.Prim.delay#) a1 a2
{-# NOINLINE waitRead# #-}
waitRead# :: Int# -> State# s -> State# s
waitRead# a1 a2 = (GHC.Prim.waitRead#) a1 a2
{-# NOINLINE waitWrite# #-}
waitWrite# :: Int# -> State# s -> State# s
waitWrite# a1 a2 = (GHC.Prim.waitWrite#) a1 a2
{-# NOINLINE fork# #-}
fork# :: a -> State# (RealWorld) -> (# State# (RealWorld),ThreadId# #)
fork# a1 a2 = (GHC.Prim.fork#) a1 a2
{-# NOINLINE forkOn# #-}
forkOn# :: Int# -> a -> State# (RealWorld) -> (# State# (RealWorld),ThreadId# #)
forkOn# a1 a2 a3 = (GHC.Prim.forkOn#) a1 a2 a3
{-# NOINLINE killThread# #-}
killThread# :: ThreadId# -> a -> State# (RealWorld) -> State# (RealWorld)
killThread# a1 a2 a3 = (GHC.Prim.killThread#) a1 a2 a3
{-# NOINLINE yield# #-}
yield# :: State# (RealWorld) -> State# (RealWorld)
yield# a1 = (GHC.Prim.yield#) a1
{-# NOINLINE myThreadId# #-}
myThreadId# :: State# (RealWorld) -> (# State# (RealWorld),ThreadId# #)
myThreadId# a1 = (GHC.Prim.myThreadId#) a1
{-# NOINLINE labelThread# #-}
labelThread# :: ThreadId# -> Addr# -> State# (RealWorld) -> State# (RealWorld)
labelThread# a1 a2 a3 = (GHC.Prim.labelThread#) a1 a2 a3
{-# NOINLINE isCurrentThreadBound# #-}
isCurrentThreadBound# :: State# (RealWorld) -> (# State# (RealWorld),Int# #)
isCurrentThreadBound# a1 = (GHC.Prim.isCurrentThreadBound#) a1
{-# NOINLINE noDuplicate# #-}
noDuplicate# :: State# (RealWorld) -> State# (RealWorld)
noDuplicate# a1 = (GHC.Prim.noDuplicate#) a1
{-# NOINLINE threadStatus# #-}
threadStatus# :: ThreadId# -> State# (RealWorld) -> (# State# (RealWorld),Int#,Int#,Int# #)
threadStatus# a1 a2 = (GHC.Prim.threadStatus#) a1 a2
{-# NOINLINE mkWeak# #-}
mkWeak# :: o -> b -> c -> State# (RealWorld) -> (# State# (RealWorld),Weak# b #)
mkWeak# a1 a2 a3 a4 = (GHC.Prim.mkWeak#) a1 a2 a3 a4
{-# NOINLINE mkWeakNoFinalizer# #-}
mkWeakNoFinalizer# :: o -> b -> State# (RealWorld) -> (# State# (RealWorld),Weak# b #)
mkWeakNoFinalizer# a1 a2 a3 = (GHC.Prim.mkWeakNoFinalizer#) a1 a2 a3
{-# NOINLINE mkWeakForeignEnv# #-}
mkWeakForeignEnv# :: o -> b -> Addr# -> Addr# -> Int# -> Addr# -> State# (RealWorld) -> (# State# (RealWorld),Weak# b #)
mkWeakForeignEnv# a1 a2 a3 a4 a5 a6 a7 = (GHC.Prim.mkWeakForeignEnv#) a1 a2 a3 a4 a5 a6 a7
{-# NOINLINE deRefWeak# #-}
deRefWeak# :: Weak# a -> State# (RealWorld) -> (# State# (RealWorld),Int#,a #)
deRefWeak# a1 a2 = (GHC.Prim.deRefWeak#) a1 a2
{-# NOINLINE finalizeWeak# #-}
finalizeWeak# :: Weak# a -> State# (RealWorld) -> (# State# (RealWorld),Int#,State# (RealWorld) -> (# State# (RealWorld),() #) #)
finalizeWeak# a1 a2 = (GHC.Prim.finalizeWeak#) a1 a2
{-# NOINLINE touch# #-}
touch# :: o -> State# (RealWorld) -> State# (RealWorld)
touch# a1 a2 = (GHC.Prim.touch#) a1 a2
{-# NOINLINE makeStablePtr# #-}
makeStablePtr# :: a -> State# (RealWorld) -> (# State# (RealWorld),StablePtr# a #)
makeStablePtr# a1 a2 = (GHC.Prim.makeStablePtr#) a1 a2
{-# NOINLINE deRefStablePtr# #-}
deRefStablePtr# :: StablePtr# a -> State# (RealWorld) -> (# State# (RealWorld),a #)
deRefStablePtr# a1 a2 = (GHC.Prim.deRefStablePtr#) a1 a2
{-# NOINLINE eqStablePtr# #-}
eqStablePtr# :: StablePtr# a -> StablePtr# a -> Int#
eqStablePtr# a1 a2 = (GHC.Prim.eqStablePtr#) a1 a2
{-# NOINLINE makeStableName# #-}
makeStableName# :: a -> State# (RealWorld) -> (# State# (RealWorld),StableName# a #)
makeStableName# a1 a2 = (GHC.Prim.makeStableName#) a1 a2
{-# NOINLINE eqStableName# #-}
eqStableName# :: StableName# a -> StableName# a -> Int#
eqStableName# a1 a2 = (GHC.Prim.eqStableName#) a1 a2
{-# NOINLINE stableNameToInt# #-}
stableNameToInt# :: StableName# a -> Int#
stableNameToInt# a1 = (GHC.Prim.stableNameToInt#) a1
{-# NOINLINE reallyUnsafePtrEquality# #-}
reallyUnsafePtrEquality# :: a -> a -> Int#
reallyUnsafePtrEquality# a1 a2 = (GHC.Prim.reallyUnsafePtrEquality#) a1 a2
{-# NOINLINE spark# #-}
spark# :: a -> State# s -> (# State# s,a #)
spark# a1 a2 = (GHC.Prim.spark#) a1 a2
{-# NOINLINE getSpark# #-}
getSpark# :: State# s -> (# State# s,Int#,a #)
getSpark# a1 = (GHC.Prim.getSpark#) a1
{-# NOINLINE numSparks# #-}
numSparks# :: State# s -> (# State# s,Int# #)
numSparks# a1 = (GHC.Prim.numSparks#) a1
{-# NOINLINE dataToTag# #-}
dataToTag# :: a -> Int#
dataToTag# a1 = (GHC.Prim.dataToTag#) a1
{-# NOINLINE addrToAny# #-}
addrToAny# :: Addr# -> (# a #)
addrToAny# a1 = (GHC.Prim.addrToAny#) a1
{-# NOINLINE mkApUpd0# #-}
mkApUpd0# :: BCO# -> (# a #)
mkApUpd0# a1 = (GHC.Prim.mkApUpd0#) a1
{-# NOINLINE newBCO# #-}
newBCO# :: ByteArray# -> ByteArray# -> Array# a -> Int# -> ByteArray# -> State# s -> (# State# s,BCO# #)
newBCO# a1 a2 a3 a4 a5 a6 = (GHC.Prim.newBCO#) a1 a2 a3 a4 a5 a6
{-# NOINLINE unpackClosure# #-}
unpackClosure# :: a -> (# Addr#,Array# b,ByteArray# #)
unpackClosure# a1 = (GHC.Prim.unpackClosure#) a1
{-# NOINLINE getApStackVal# #-}
getApStackVal# :: a -> Int# -> (# Int#,b #)
getApStackVal# a1 a2 = (GHC.Prim.getApStackVal#) a1 a2
{-# NOINLINE getCCSOf# #-}
getCCSOf# :: a -> State# s -> (# State# s,Addr# #)
getCCSOf# a1 a2 = (GHC.Prim.getCCSOf#) a1 a2
{-# NOINLINE getCurrentCCS# #-}
getCurrentCCS# :: a -> State# s -> (# State# s,Addr# #)
getCurrentCCS# a1 a2 = (GHC.Prim.getCurrentCCS#) a1 a2
{-# NOINLINE traceEvent# #-}
traceEvent# :: Addr# -> State# s -> State# s
traceEvent# a1 a2 = (GHC.Prim.traceEvent#) a1 a2
