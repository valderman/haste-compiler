{-# LANGUAGE CPP #-}
module Haste.PrimOps (genOp) where
import Prelude hiding (LT, GT)
import PrimOp
import Data.JSTarget
import Haste.Config
import Haste.Util

-- | Dummy State# RealWorld value for where one is needed.
defState :: AST Exp
defState = litN 0

-- | Generate primops.
--   Many of these ops return lifted Bool values; however, no thunk is
--   generated for them in order to conserve space and CPU time. This relies
--   on the evaluation operation in the RTS being able to handle plain values
--   as though they were thunks. If this were to change, all those ops MUST
--   be changed to return thunks!
genOp :: Config -> PrimOp -> [AST Exp] -> Either String (AST Exp)
genOp cfg op xs =
  case op of
    -- negations
    IntNegOp       -> Right $ binOp Sub (litN 0) (head xs)
    DoubleNegOp    -> Right $ binOp Sub (litN 0) (head xs)
    FloatNegOp     -> Right $ binOp Sub (litN 0) (head xs)
    NotOp          -> Right $ not_ (head xs) -- bitwise

    -- Conversions
    ChrOp          -> Right $ head xs
    OrdOp          -> Right $ head xs
    Word2IntOp     -> Right $ binOp BitAnd (head xs) (litN 0xffffffff)
    Int2WordOp     -> Right $ binOp ShrL (head xs) (litN 0)
    Int2FloatOp    -> Right $ head xs
    Int2DoubleOp   -> Right $ head xs
    Double2IntOp   -> Right $ binOp (BitAnd) (head xs) (litN 0xffffffff)
    Double2FloatOp -> Right $ head xs
    Float2IntOp    -> Right $ binOp (BitAnd) (head xs) (litN 0xffffffff)
    Float2DoubleOp -> Right $ head xs
    
    -- Narrowing ops
    Narrow8IntOp   -> Right $ binOp BitAnd (head xs) (lit (0xff :: Double))
    Narrow16IntOp  -> Right $ binOp BitAnd (head xs) (lit (0xffff :: Double))
    Narrow32IntOp  -> Right $ binOp BitAnd (head xs) (lit (0xffffffff :: Double))
    Narrow8WordOp  -> Right $ binOp BitAnd (head xs) (lit (0xff :: Double))
    Narrow16WordOp -> Right $ binOp BitAnd (head xs) (lit (0xffff :: Double))
    Narrow32WordOp -> Right $ binOp ShrL (binOp BitAnd (head xs) (lit (0xffffffff :: Double))) (litN 0)

    -- Char ops
    CharGtOp -> bOp GT
    CharGeOp -> bOp GTE
    CharEqOp -> bOp Eq
    CharNeOp -> bOp Neq
    CharLtOp -> bOp LT
    CharLeOp -> bOp LTE

    -- Int ops
    IntAddOp ->        intMath $ bOp Add
    IntSubOp ->        intMath $ bOp Sub
    IntMulOp ->        intMath $ Right $ multiplyIntOp cfg (xs !! 0) (xs !! 1)
    -- FIXME: this is correct but slow!
    IntMulMayOfloOp -> intMath $ Right $ multiplyIntOp cfg (xs !! 0) (xs !! 1)
    IntQuotOp ->       callF "quot"
#if __GLASGOW_HASKELL__ >= 706
    IntQuotRemOp ->    callF "quotRemI"
#endif
    IntRemOp ->        bOp Mod -- JS % operator is actually rem, not mod!
    IntAddCOp -> callF "addC"
    IntSubCOp -> callF "subC"
    ISllOp ->    bOp Shl
    ISraOp ->    bOp ShrA
    ISrlOp ->    bOp ShrL
    IntGtOp ->   bOp GT
    IntGeOp ->   bOp GTE
    IntLtOp ->   bOp LT
    IntLeOp ->   bOp LTE
    IntEqOp ->   bOp Eq
    IntNeOp ->   bOp Neq

    -- Word ops
    WordAddOp ->  wordMath $ bOp Add
    WordSubOp ->  wordMath $ bOp Sub
    WordMulOp ->  wordMath $ callF "imul"
    WordQuotOp -> callF "quot"
#if __GLASGOW_HASKELL__ >= 706
    WordQuotRemOp -> callF "quotRemI"
#endif
    WordRemOp ->  bOp Mod
    AndOp ->      wordMath $ bOp BitAnd
    OrOp ->       wordMath $ bOp BitOr
    XorOp ->      wordMath $ bOp BitXor
    SllOp ->      wordMath $ bOp Shl
    SrlOp ->      bOp ShrL
    WordGtOp ->   bOp GT
    WordGeOp ->   bOp GTE
    WordEqOp ->   bOp Eq
    WordNeOp ->   bOp Neq
    WordLtOp ->   bOp LT
    WordLeOp ->   bOp LTE

    -- Double ops
    DoubleExpOp    -> Right $ callForeign "Math.exp" xs
    DoubleLogOp    -> Right $ callForeign "Math.log" xs
    DoubleSqrtOp   -> Right $ callForeign "Math.sqrt" xs
    DoubleCosOp    -> Right $ callForeign "Math.cos" xs
    DoubleSinOp    -> Right $ callForeign "Math.sin" xs
    DoubleTanOp    -> Right $ callForeign "Math.tan" xs
    DoubleAcosOp   -> Right $ callForeign "Math.acos" xs
    DoubleAsinOp   -> Right $ callForeign "Math.asin" xs
    DoubleAtanOp   -> Right $ callForeign "Math.atan" xs
    DoubleCoshOp   -> Right $ callForeign "cosh" xs
    DoubleSinhOp   -> Right $ callForeign "sinh" xs
    DoubleTanhOp   -> Right $ callForeign "tanh" xs
    DoubleDecode_2IntOp -> Right $ callForeign "decodeDouble" xs
    DoubleGtOp ->    bOp GT
    DoubleGeOp ->    bOp GTE
    DoubleEqOp ->    bOp Eq
    DoubleNeOp ->    bOp Neq
    DoubleLtOp ->    bOp LT
    DoubleLeOp ->    bOp LTE
    DoubleAddOp ->   bOp Add
    DoubleSubOp ->   bOp Sub
    DoubleMulOp ->   bOp Mul
    DoubleDivOp ->   bOp Div
    DoublePowerOp -> callF "Math.pow"

    -- Float ops
    FloatExpOp     -> Right $ callForeign "Math.exp" xs
    FloatLogOp     -> Right $ callForeign "Math.log" xs
    FloatSqrtOp    -> Right $ callForeign "Math.sqrt" xs
    FloatCosOp     -> Right $ callForeign "Math.cos" xs
    FloatSinOp     -> Right $ callForeign "Math.sin" xs
    FloatTanOp     -> Right $ callForeign "Math.tan" xs
    FloatAcosOp    -> Right $ callForeign "Math.acos" xs
    FloatAsinOp    -> Right $ callForeign "Math.asin" xs
    FloatAtanOp    -> Right $ callForeign "Math.atan" xs
    FloatCoshOp    -> Right $ callForeign "cosh" xs
    FloatSinhOp    -> Right $ callForeign "sinh" xs
    FloatTanhOp    -> Right $ callForeign "tanh" xs
    FloatDecode_IntOp -> Right $ callForeign "decodeFloat" xs
    FloatGtOp ->  bOp GT
    FloatGeOp ->  bOp GTE
    FloatEqOp ->  bOp Eq
    FloatNeOp ->  bOp Neq
    FloatLtOp ->  bOp LT
    FloatLeOp ->  bOp LTE
    FloatAddOp -> bOp Add
    FloatSubOp -> bOp Sub
    FloatMulOp -> bOp Mul
    FloatDivOp -> bOp Div
    FloatPowerOp -> callF "Math.pow"
    
    -- Array ops
    NewArrayOp -> callF "newArr"
    SameMutableArrayOp -> fmap (thunk . ret) $ bOp Eq
    ReadArrayOp -> Right $ index arr ix
    WriteArrayOp -> Right $ assignEx (index arr ix) rhs
      where (_arr:_ix:rhs:_) = xs
    SizeofArrayOp -> Right $ index (head xs) (lit "length")
    SizeofMutableArrayOp -> Right $ index (head xs) (lit "length")
    IndexArrayOp -> Right $ index arr ix
    UnsafeFreezeArrayOp -> Right $ head xs
    UnsafeThawArrayOp -> Right $ head xs
    -- TODO: copy, clone, freeze, thaw
    
    -- Byte Array ops
    NewByteArrayOp_Char      -> callF "newByteArr"
    NewPinnedByteArrayOp_Char-> callF "newByteArr"
    SameMutableByteArrayOp   -> fmap (thunk . ret) $ bOp Eq
    IndexByteArrayOp_Char    -> readArr xs "i8"
    IndexByteArrayOp_Int     -> readArr xs "i32"
    IndexByteArrayOp_Int8    -> readArr xs "i8"
    IndexByteArrayOp_Int16   -> readArr xs "i16"
    IndexByteArrayOp_Int32   -> readArr xs "i32"
    IndexByteArrayOp_Word    -> readArr xs "w32"
    IndexByteArrayOp_Word8   -> readArr xs "w8"
    IndexByteArrayOp_Word16  -> readArr xs "w16"
    IndexByteArrayOp_Word32  -> readArr xs "w32"
    IndexByteArrayOp_WideChar-> readArr xs "w32"
    IndexByteArrayOp_Float   -> readArr xs "f32"
    IndexByteArrayOp_Double  -> readArr xs "f64"
    
    ReadByteArrayOp_Char     -> readArr xs "i8"
    ReadByteArrayOp_Int      -> readArr xs "i32"
    ReadByteArrayOp_Int8     -> readArr xs "i8"
    ReadByteArrayOp_Int16    -> readArr xs "i16"
    ReadByteArrayOp_Int32    -> readArr xs "i32"
    ReadByteArrayOp_Word     -> readArr xs "w32"
    ReadByteArrayOp_Word8    -> readArr xs "w8"
    ReadByteArrayOp_Word16   -> readArr xs "w16"
    ReadByteArrayOp_Word32   -> readArr xs "w32"
    ReadByteArrayOp_WideChar -> readArr xs "w32"
    ReadByteArrayOp_Float    -> readArr xs "f32"
    ReadByteArrayOp_Double   -> readArr xs "f64"
    
    WriteByteArrayOp_Char    -> writeArr xs "i8"
    WriteByteArrayOp_Int     -> writeArr xs "i32"
    WriteByteArrayOp_Int8    -> writeArr xs "i8"
    WriteByteArrayOp_Int16   -> writeArr xs "i16"
    WriteByteArrayOp_Int32   -> writeArr xs "i32"
    WriteByteArrayOp_Word    -> writeArr xs "w32"
    WriteByteArrayOp_Word8   -> writeArr xs "w8"
    WriteByteArrayOp_Word16  -> writeArr xs "w16"
    WriteByteArrayOp_Word32  -> writeArr xs "w32"
    WriteByteArrayOp_WideChar-> writeArr xs "w32"
    WriteByteArrayOp_Float   -> writeArr xs "f32"
    WriteByteArrayOp_Double  -> writeArr xs "f64"
    
    SizeofByteArrayOp        -> Right $ index (head xs) (lit "byteLength")
    SizeofMutableByteArrayOp -> Right $ index (head xs) (lit "byteLength")
    NewAlignedPinnedByteArrayOp_Char -> Right $ callForeign "newByteArr" [xs!!0]
    UnsafeFreezeByteArrayOp  -> Right $ head xs
    ByteArrayContents_Char   -> Right $ head xs
    
    -- Mutable variables
    NewMutVarOp -> callF "nMV"
    ReadMutVarOp -> callF "rMV"
    WriteMutVarOp -> callF "wMV"
    SameMutVarOp -> bOp Eq
    AtomicModifyMutVarOp -> callF "mMV"
    
    -- TVars - since there's no parallelism and no preemption, TVars behave
    -- just like normal IORefs.
    NewTVarOp   -> callF "nMV"
    ReadTVarOp  -> callF "rMV"
    WriteTVarOp -> callF "wMV"
    SameTVarOp  -> bOp Eq

    -- Pointer ops
    ReallyUnsafePtrEqualityOp -> bOp Eq
    WriteOffAddrOp_Char    -> writeOffAddr xs "i8"  1
    WriteOffAddrOp_Int     -> writeOffAddr xs "i32" 4
    WriteOffAddrOp_Int8    -> writeOffAddr xs "i8"  1
    WriteOffAddrOp_Int16   -> writeOffAddr xs "i16" 2
    WriteOffAddrOp_Int32   -> writeOffAddr xs "i32" 4
    WriteOffAddrOp_Word    -> writeOffAddr xs "w32" 4
    WriteOffAddrOp_Word8   -> writeOffAddr xs "w8"  1
    WriteOffAddrOp_Word16  -> writeOffAddr xs "w16" 2
    WriteOffAddrOp_Word32  -> writeOffAddr xs "w32" 4
    WriteOffAddrOp_WideChar-> writeOffAddr xs "w32" 4
    WriteOffAddrOp_Float   -> writeOffAddr xs "f32" 4
    WriteOffAddrOp_Double  -> writeOffAddr xs "f64" 8
    ReadOffAddrOp_Char     -> readOffAddr xs "i8"   1
    ReadOffAddrOp_Int      -> readOffAddr xs "i32"  4
    ReadOffAddrOp_Int8     -> readOffAddr xs "i8"   1
    ReadOffAddrOp_Int16    -> readOffAddr xs "i16"  2
    ReadOffAddrOp_Int32    -> readOffAddr xs "i32"  4
    ReadOffAddrOp_Word     -> readOffAddr xs "w32"  4
    ReadOffAddrOp_Word8    -> readOffAddr xs "w8"   1
    ReadOffAddrOp_Word16   -> readOffAddr xs "w16"  2
    ReadOffAddrOp_Word32   -> readOffAddr xs "w32"  4
    ReadOffAddrOp_WideChar -> readOffAddr xs "w32"  4
    ReadOffAddrOp_Float    -> readOffAddr xs "f32"  4
    ReadOffAddrOp_Double   -> readOffAddr xs "f64"  8
    AddrAddOp              -> callF "plusAddr"
    AddrSubOp              ->
        Right $ callForeign "plusAddr" [addr, binOp Sub (litN 0) off]
      where (addr:off:_) = xs
    AddrEqOp               -> callF "addrEq"
    AddrNeOp               ->
        Right $ binOp Sub (litN 0) $ callForeign "addrEq" [a, b]
      where (a:b:_) = xs
    AddrLtOp               -> callF "addrLT"
    AddrGtOp               -> callF "addrGT"
    AddrLeOp               ->
        Right $ binOp Sub (litN 0) $ callForeign "addrGT" [a, b]
      where (a:b:_) = xs
    AddrGeOp               ->
        Right $ binOp Sub (litN 0) $ callForeign "addrLT" [a, b]
      where (a:b:_) = xs
    Addr2IntOp             ->
        Right $ index x (lit "off")
      where
        (x:_) = xs

    -- MVars
    NewMVarOp     -> callF "newMVar"
    TakeMVarOp    -> callF "takeMVar"
    TryTakeMVarOp -> callF "tryTakeMVar"
    PutMVarOp     -> callF "putMVar"
    TryPutMVarOp  -> callF "tryPutMVar"
    SameMVarOp    -> callF "sameMVar"
    IsEmptyMVarOp -> callF "isEmptyMVar"

    -- Stable names
    MakeStableNameOp  -> callF "makeStableName"
    EqStableNameOp    -> callF "eqStableName"
    StableNameToIntOp -> Right $ head xs

    -- Stable pointers - all pointers are stable in JS!
    MakeStablePtrOp   -> Right $ head xs
    EqStablePtrOp     -> bOp Eq
    DeRefStablePtrOp  -> Right $ head xs

    -- Exception masking
    -- There's only one thread anyway, so async exceptions can't happen.
    MaskAsyncExceptionsOp   -> Right $ callSaturated (head xs) []
    UnmaskAsyncExceptionsOp -> Right $ callSaturated (head xs) []
    MaskStatus              -> Right $ litN 0

    -- Misc. ops
    PopCntOp       -> Right $ callForeign "popCnt" [head xs]
    PopCnt8Op      -> Right $ callForeign "popCnt" [head xs]
    PopCnt16Op     -> Right $ callForeign "popCnt" [head xs]
    PopCnt32Op     -> Right $ callForeign "popCnt" [head xs]
    DelayOp        -> Right $ defState
    SeqOp          -> Right $ callForeign "E" [head xs]
    AtomicallyOp   -> Right $ callSaturated (xs !! 0) []
    -- Get the data constructor tag from a value.
    DataToTagOp    -> callF "dataToTag"
    -- Basically unsafeCoerce :: Int# -> <enumeration type>
    TagToEnumOp    -> Right $ head xs
    TouchOp        -> Right $ defState
    RaiseOp        -> callF "die"
    RaiseIOOp      -> callF "die"
    -- noDuplicate is only relevant in a threaded environment.
    NoDuplicateOp  -> Right $ defState
    CatchOp        -> callF "jsCatch"
    x              -> Left $ "Unsupported PrimOp: " ++ showOutputable x
  where
    (arr:ix:_) = xs
    
    writeArr (a:i:rhs:_) elemtype =
      Right $ assignEx (index (index (index a (lit "v")) (lit elemtype)) i) rhs
    writeArr _ _ =
      error "writeArray primop with too few arguments!"

    readArr (a:i:_) elemtype =
      Right $ index (index (index a (lit "v")) (lit elemtype)) i
    readArr _ _ =
      error "writeArray primop with too few arguments!"

    writeOffAddr (addr:off:rhs:_) etype esize =
      Right $ callForeign "writeOffAddr" [lit etype, litN esize, addr, off, rhs]
    writeOffAddr _ _ _ =
      error "writeOffAddr primop with too few arguments!"
    
    readOffAddr (addr:off:_) etype esize =
      Right $ callForeign "readOffAddr" [lit etype, litN esize, addr, off]
    readOffAddr _ _ _ =
      error "readOffAddr primop with too few arguments!"

    callF f = Right $ callForeign f xs
    
    bOp bop =
      case xs of
        [x, y] -> Right $ binOp bop x y
        _      -> error $ "PrimOps.binOp failed! op is " ++ show bop
    
    -- Bitwise ops on words need to be unsigned; exploit the fact that >>> is!
    wordMath = fmap (\oper -> binOp ShrL oper (litN 0))
    intMath = fmap (wrapIntMath cfg)
