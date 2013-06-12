module CodeGen.Javascript.PrimOps (genOp) where
import PrimOp
import CodeGen.Javascript.AST as AST
import CodeGen.Javascript.Config
import CodeGen.Javascript.Util

-- | Generate primops.
--   Many of these ops return lifted Bool values; however, no thunk is
--   generated for them in order to conserve space and CPU time. This relies
--   on the evaluation operation in the RTS being able to handle plain values
--   as though they were thunks. If this were to change, all those ops MUST
--   be changed to return thunks!
genOp :: Config -> PrimOp -> [JSExp] -> Either String JSExp
genOp cfg op xs =
  case op of
    -- Negations
    IntNegOp       -> Right $ Neg (head xs)
    DoubleNegOp    -> Right $ Neg (head xs)
    FloatNegOp     -> Right $ Neg (head xs)
    NotOp          -> Right $ Not (head xs) -- bitwise

    -- Conversions
    ChrOp          -> Right $ head xs
    OrdOp          -> Right $ head xs
    Word2IntOp     -> Right $ BinOp BitAnd (head xs) (litN 0xffffffff)
    Int2WordOp     -> Right $ BinOp ShrL (head xs) (litN 0)
    Int2FloatOp    -> Right $ head xs
    Int2DoubleOp   -> Right $ head xs
    Double2IntOp   -> Right $ BinOp (BitAnd) (head xs) (litN 0xffffffff)
    Double2FloatOp -> Right $ head xs
    Float2IntOp    -> Right $ BinOp (BitAnd) (head xs) (litN 0xffffffff)
    Float2DoubleOp -> Right $ head xs
    
    -- Narrowing ops
    Narrow8IntOp   -> Right $ BinOp BitAnd (head xs) (lit (0xff :: Double))
    Narrow16IntOp  -> Right $ BinOp BitAnd (head xs) (lit (0xffff :: Double))
    Narrow32IntOp  -> Right $ BinOp BitAnd (head xs) (lit (0xffffffff :: Double))
    Narrow8WordOp  -> Right $ BinOp BitAnd (head xs) (lit (0xff :: Double))
    Narrow16WordOp -> Right $ BinOp BitAnd (head xs) (lit (0xffff :: Double))
    Narrow32WordOp -> Right $ BinOp ShrL (BinOp BitAnd (head xs) (lit (0xffffffff :: Double))) (litN 0)

    -- Char ops
    CharGtOp -> binOp AST.GT
    CharGeOp -> binOp GTE
    CharEqOp -> binOp Eq
    CharNeOp -> binOp Neq
    CharLtOp -> binOp AST.LT
    CharLeOp -> binOp LTE

    -- Int ops
    IntAddOp ->        intMath $ binOp Add
    IntSubOp ->        intMath $ binOp Sub
    IntMulOp ->        intMath $ Right $ multiplyIntOp cfg (xs !! 0) (xs !! 1)
    -- FIXME: this is correct but slow!
    IntMulMayOfloOp -> intMath $ Right $ multiplyIntOp cfg (xs !! 0) (xs !! 1)
    IntQuotOp ->       call "quot"
    IntQuotRemOp ->    call "quotRemI"
    IntRemOp ->        binOp Mod -- JS % operator is actually rem, not mod!
    IntAddCOp -> call "addC"
    IntSubCOp -> call "subC"
    ISllOp ->    binOp Shl
    ISraOp ->    binOp ShrA
    ISrlOp ->    binOp ShrL
    IntGtOp ->   binOp AST.GT
    IntGeOp ->   binOp GTE
    IntLtOp ->   binOp AST.LT
    IntLeOp ->   binOp LTE
    IntEqOp ->   binOp Eq
    IntNeOp ->   binOp Neq

    -- Word ops
    WordAddOp ->  wordMath $ binOp Add
    WordSubOp ->  wordMath $ binOp Sub
    WordMulOp ->  wordMath $ call "imul"
    WordQuotOp -> call "quot"
    WordRemOp ->  binOp Mod
    AndOp ->      wordMath $ binOp BitAnd
    OrOp ->       wordMath $ binOp BitOr
    XorOp ->      wordMath $ binOp BitXor
    SllOp ->      wordMath $ binOp Shl
    SrlOp ->      binOp ShrL
    WordGtOp ->   binOp AST.GT
    WordGeOp ->   binOp GTE
    WordEqOp ->   binOp Eq
    WordNeOp ->   binOp Neq
    WordLtOp ->   binOp AST.LT
    WordLeOp ->   binOp LTE

    -- Double ops
    DoubleExpOp    -> Right $ NativeCall "Math.exp" xs
    DoubleLogOp    -> Right $ NativeCall "Math.log" xs
    DoubleSqrtOp   -> Right $ NativeCall "Math.sqrt" xs
    DoubleCosOp    -> Right $ NativeCall "Math.cos" xs
    DoubleSinOp    -> Right $ NativeCall "Math.sin" xs
    DoubleTanOp    -> Right $ NativeCall "Math.tan" xs
    DoubleAcosOp   -> Right $ NativeCall "Math.acos" xs
    DoubleAsinOp   -> Right $ NativeCall "Math.asin" xs
    DoubleAtanOp   -> Right $ NativeCall "Math.atan" xs
    DoubleCoshOp   -> Right $ NativeCall "cosh" xs
    DoubleSinhOp   -> Right $ NativeCall "sinh" xs
    DoubleTanhOp   -> Right $ NativeCall "tanh" xs
    DoubleDecode_2IntOp -> Right $ NativeCall "decodeDouble" xs
    DoubleGtOp ->    binOp AST.GT
    DoubleGeOp ->    binOp GTE
    DoubleEqOp ->    binOp Eq
    DoubleNeOp ->    binOp Neq
    DoubleLtOp ->    binOp AST.LT
    DoubleLeOp ->    binOp LTE
    DoubleAddOp ->   binOp Add
    DoubleSubOp ->   binOp Sub
    DoubleMulOp ->   binOp Mul
    DoubleDivOp ->   binOp Div
    DoublePowerOp -> call "Math.pow"

    -- Float ops
    FloatExpOp     -> Right $ NativeCall "Math.exp" xs
    FloatLogOp     -> Right $ NativeCall "Math.log" xs
    FloatSqrtOp    -> Right $ NativeCall "Math.sqrt" xs
    FloatCosOp     -> Right $ NativeCall "Math.cos" xs
    FloatSinOp     -> Right $ NativeCall "Math.sin" xs
    FloatTanOp     -> Right $ NativeCall "Math.tan" xs
    FloatAcosOp    -> Right $ NativeCall "Math.acos" xs
    FloatAsinOp    -> Right $ NativeCall "Math.asin" xs
    FloatAtanOp    -> Right $ NativeCall "Math.atan" xs
    FloatCoshOp    -> Right $ NativeCall "cosh" xs
    FloatSinhOp    -> Right $ NativeCall "sinh" xs
    FloatTanhOp    -> Right $ NativeCall "tanh" xs
    FloatDecode_IntOp -> Right $ NativeCall "decodeFloat" xs
    FloatGtOp ->  binOp AST.GT
    FloatGeOp ->  binOp GTE
    FloatEqOp ->  binOp Eq
    FloatNeOp ->  binOp Neq
    FloatLtOp ->  binOp AST.LT
    FloatLeOp ->  binOp LTE
    FloatAddOp -> binOp Add
    FloatSubOp -> binOp Sub
    FloatMulOp -> binOp Mul
    FloatDivOp -> binOp Div
    FloatPowerOp -> call "Math.pow"
    
    -- Array ops
    NewArrayOp -> call "newArr"
    SameMutableArrayOp -> fmap (Thunk []) $ binOp Eq
    ReadArrayOp -> Right $ Index arr ix
    WriteArrayOp -> Right $ Assign (Index arr ix) rhs
      where (_arr:_ix:rhs:_) = xs
    SizeofArrayOp -> Right $ Index (head xs) (lit "length")
    SizeofMutableArrayOp -> Right $ Index (head xs) (lit "length")
    IndexArrayOp -> Right $ Index arr ix
    UnsafeFreezeArrayOp -> Right $ head xs
    UnsafeThawArrayOp -> Right $ head xs
    -- TODO: copy, clone, freeze, thaw
    
    -- Byte Array ops
    NewByteArrayOp_Char      -> call "newByteArr"
    NewPinnedByteArrayOp_Char-> call "newByteArr"
    SameMutableByteArrayOp   -> fmap (Thunk []) $ binOp Eq
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
    
    SizeofByteArrayOp        -> Right $ Index (head xs) (lit "byteLength")
    SizeofMutableByteArrayOp -> Right $ Index (head xs) (lit "byteLength")
    NewAlignedPinnedByteArrayOp_Char -> Right $ NativeCall "newByteArr" [xs!!0]
    UnsafeFreezeByteArrayOp  -> Right $ head xs
    ByteArrayContents_Char   -> Right $ head xs
    
    -- Mutable variables
    NewMutVarOp -> call "nMV"
    ReadMutVarOp -> call "rMV"
    WriteMutVarOp -> call "wMV"
    
    -- Pointer ops
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
    AddrAddOp              -> call "plusAddr"
    AddrSubOp              -> Right $ NativeCall "plusAddr" [addr, Neg off]
      where (addr:off:_) = xs
    AddrEqOp               -> call "addrEq"
    AddrNeOp               -> Right $ Neg $ NativeCall "addrEq" [a, b]
      where (a:b:_) = xs
    AddrLtOp               -> call "addrLT"
    AddrGtOp               -> call "addrGT"
    AddrLeOp               -> Right $ Neg $ NativeCall "addrGT" [a, b]
      where (a:b:_) = xs
    AddrGeOp               -> Right $ Neg $ NativeCall "addrLT" [a, b]
      where (a:b:_) = xs

    -- MVars
    NewMVarOp     -> call "newMVar"
    TakeMVarOp    -> call "takeMVar"
    TryTakeMVarOp -> call "tryTakeMVar"
    PutMVarOp     -> call "putMVar"
    TryPutMVarOp  -> call "tryPutMVar"
    SameMVarOp    -> call "sameMVar"
    IsEmptyMVarOp -> call "isEmptyMVar"

    -- Stable names
    MakeStableNameOp  -> call "makeStableName"
    EqStableNameOp    -> call "eqStableName"
    StableNameToIntOp -> Right $ head xs

    -- Exception masking
    -- There's only one thread anyway, so async exceptions can't happen.
    MaskAsyncExceptionsOp   -> Right $ Call (head xs) []
    UnmaskAsyncExceptionsOp -> Right $ Call (head xs) []
    MaskStatus              -> Right $ litN 0

    -- Misc. ops
    SeqOp          -> Right $ NativeCall "E" [head xs]
    AtomicallyOp   -> Right $ Call (xs !! 0) []
    -- Get the data constructor tag from a value.
    DataToTagOp    -> call "dataToTag"
    TouchOp        -> Right $ defState
    RaiseOp        -> call "die"
    RaiseIOOp      -> call "die"
    -- noDuplicate is only relevant in a threaded environment.
    NoDuplicateOp  -> Right $ defState
    CatchOp        -> call "jsCatch"
    x              -> Left $ "Unsupported PrimOp: " ++ showOutputable x
  where
    (arr:ix:_) = xs
    
    writeArr (a:i:rhs:_) elemtype =
      Right $ Assign (Index (Index (Index a (lit "v")) (lit elemtype)) i) rhs
    writeArr _ _ =
      error "writeArray primop with too few arguments!"

    readArr (a:i:_) elemtype =
      Right $ Index (Index (Index a (lit "v")) (lit elemtype)) i
    readArr _ _ =
      error "writeArray primop with too few arguments!"

    writeOffAddr (addr:off:rhs:_) etype esize =
      Right $ NativeCall "writeOffAddr" [lit etype, litN esize, addr, off, rhs]
    writeOffAddr _ _ _ =
      error "writeOffAddr primop with too few arguments!"
    
    readOffAddr (addr:off:_) etype esize =
      Right $ NativeCall "readOffAddr" [lit etype, litN esize, addr, off]
    readOffAddr _ _ _ =
      error "readOffAddr primop with too few arguments!"

    call f = Right $ NativeCall f xs
    
    binOp bop =
      case xs of
        [x, y] -> Right $ BinOp bop x y
        _      -> error $ "PrimOps.binOp failed! op is " ++ show bop
    
    -- Bitwise ops on words need to be unsigned; exploit the fact that >>> is!
    wordMath = fmap (\oper -> BinOp ShrL oper (litN 0))
    intMath = fmap (wrapIntMath cfg)
