module CodeGen.Javascript.PrimOps (genOp) where
import PrimOp
import CodeGen.Javascript.AST as AST
import CodeGen.Javascript.Config

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
    ChrOp          -> Right $ NativeCall "String.fromCharCode" xs
    OrdOp          -> Right $ NativeMethCall (head xs) "charCodeAt" [lit (0::Double)]
    Word2IntOp     -> Right $ BinOp BitAnd (head xs) (litN 0xffffffff)
    Int2WordOp     -> Right $ BinOp ShrL (head xs) (litN 0)
    Int2FloatOp    -> Right $ head xs
    Int2DoubleOp   -> Right $ head xs
    Double2IntOp   -> Right $ head xs
    Double2FloatOp -> Right $ head xs
    Float2IntOp    -> Right $ head xs
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
    ReadArrayOp -> Right $ Array [defTag, defState, Index arr ix]
      where (arr:ix:_) = xs
    WriteArrayOp -> Right $ Assign (Index arr ix) rhs
      where (arr:ix:rhs:_) = xs
    SizeofArrayOp -> Right $ Index (head xs) (lit "length")
    SizeofMutableArrayOp -> Right $ Index (head xs) (lit "length")
    IndexArrayOp -> Right $ Array [defTag, Index arr ix]
      where (arr:ix:_) = xs
    UnsafeFreezeArrayOp -> Right $ Array [defTag, defState, (head xs)]
    UnsafeThawArrayOp -> Right $ Array [defTag, defState, (head xs)]
    -- TODO: copy, clone, freeze, thaw
    
    -- Mutable variables
    NewMutVarOp -> call "nMV"
    ReadMutVarOp -> call "rMV"
    WriteMutVarOp -> call "wMV"
    
    -- OffAddr ops
    ReadOffAddrOp_Char -> Right $ Index (xs !! 0) (xs !! 1)
    ReadOffAddrOp_Int8 -> Right $ Index (xs !! 0) (xs !! 1)
    ReadOffAddrOp_Word8 -> Right $ Index (xs !! 0) (xs !! 1)
    ReadOffAddrOp_WideChar -> Right $ Index (xs !! 0) (xs !! 1)
    WriteOffAddrOp_WideChar -> call "wOffAddr"
    WriteOffAddrOp_Int8 -> call "wOffAddr"

    -- ByteArray ops
    NewAlignedPinnedByteArrayOp_Char -> Right $ NativeCall "newBA" [xs!!0, xs!!2]
    UnsafeFreezeByteArrayOp -> Right $ Array $ [litN 1,xs!!1,xs!!0]
    ByteArrayContents_Char -> Right $ head xs

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

    -- Misc. ops
    SeqOp          -> Right $ Array [litN 1, xs !! 1, NativeCall "E" [(xs!!0)]]
    AtomicallyOp   -> Right $ Call (xs !! 0) [xs !! 1]
    -- Get the data constructor tag from a value.
    DataToTagOp    -> call "dataToTag"
    TouchOp        -> Right $ xs !! 1
    RaiseOp        -> call "die"
    RaiseIOOp      -> call "die"
    -- noDuplicate is only relevant in a threaded environment.
    NoDuplicateOp  -> Right $ head xs
    CatchOp        -> call "jsCatch"
    x              -> Left $ "Unsupported PrimOp: " ++ show x
  where
    call f = Right $ NativeCall f xs
    binOp bop = let [x, y] = xs in Right $ BinOp bop x y
    
    -- Bitwise ops on words need to be unsigned; exploit the fact that >>> is!
    wordMath = fmap (\op -> BinOp ShrL op (litN 0))
    intMath = fmap (wrapIntMath cfg)
