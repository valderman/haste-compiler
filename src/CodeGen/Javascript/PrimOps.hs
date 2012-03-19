module CodeGen.Javascript.PrimOps (genOp) where
import PrimOp
import CodeGen.Javascript.AST as AST

runtimeError :: String -> JSExp
runtimeError s = NativeCall "die" [lit $ "\"" ++ s ++ "\""]

-- | Generate primops.
--   Many of these ops return lifted Bool values; however, no thunk is
--   generated for them in order to conserve space and CPU time. This relies
--   on the evaluation operation in the RTS being able to handle plain values
--   as though they were thunks. If this were to change, all those ops MUST
--   be changed to return thunks!
genOp :: PrimOp -> [JSExp] -> JSExp
genOp op xs =
  case op of
    -- Negations
    IntNegOp       -> Neg (head xs)
    DoubleNegOp    -> Neg (head xs)
    FloatNegOp     -> Neg (head xs)
    NotOp          -> Not (head xs) -- bitwise

    -- Conversions
    ChrOp          -> NativeCall "String.fromCharCode" xs
    OrdOp          -> NativeMethCall (head xs) "charCodeAt" [lit (0::Double)]
    Word2IntOp     -> head xs
    Int2WordOp     -> head xs
    Int2FloatOp    -> head xs
    Int2DoubleOp   -> head xs
    Double2IntOp   -> head xs
    Double2FloatOp -> head xs
    Float2IntOp    -> head xs
    Float2DoubleOp -> head xs
    
    -- Narrowing ops
    Narrow8IntOp   -> BinOp And (head xs) (lit (0xff :: Double))
    Narrow16IntOp  -> BinOp And (head xs) (lit (0xffff :: Double))
    Narrow32IntOp  -> BinOp And (head xs) (lit (0xffffffff :: Double))
    Narrow8WordOp  -> BinOp And (head xs) (lit (0xff :: Double))
    Narrow16WordOp -> BinOp And (head xs) (lit (0xffff :: Double))
    Narrow32WordOp -> BinOp And (head xs) (lit (0xffffffff :: Double))

    -- Char ops
    CharGtOp -> binOp AST.GT
    CharGeOp -> binOp GTE
    CharEqOp -> binOp Eq
    CharNeOp -> binOp Neq
    CharLtOp -> binOp AST.LT
    CharLeOp -> binOp LTE

    -- Int ops
    IntAddOp -> binOp Add
    IntSubOp -> binOp Sub
    IntMulOp -> binOp Mul
    IntMulMayOfloOp -> binOp Mul -- This is correct, but slow!
    IntQuotOp -> call "quot"
    IntRemOp -> binOp Mod -- Javascript % operator is actually rem, not mod!
    IntAddCOp -> call "addC"
    IntSubCOp -> call "subC"
    ISllOp -> binOp Shl
    ISraOp -> binOp ShrA
    ISrlOp -> binOp ShrL
    IntGtOp -> binOp AST.GT
    IntGeOp -> binOp GTE
    IntLtOp -> binOp AST.LT
    IntLeOp -> binOp LTE
    IntEqOp -> binOp Eq
    IntNeOp -> binOp Neq

    -- Word ops
    WordAddOp -> binOp Add
    WordSubOp -> binOp Sub
    WordMulOp -> binOp Mul
    WordQuotOp -> call "quot"
    WordRemOp -> binOp Mod
    AndOp -> binOp BitAnd
    OrOp -> binOp BitOr
    XorOp -> binOp BitXor
    SllOp -> binOp Shl
    SrlOp -> binOp ShrL
    WordGtOp -> binOp AST.GT
    WordGeOp -> binOp GTE
    WordEqOp -> binOp Eq
    WordNeOp -> binOp Neq
    WordLtOp -> binOp AST.LT
    WordLeOp -> binOp LTE

    -- Double ops
    DoubleExpOp    -> NativeCall "Math.exp" xs
    DoubleLogOp    -> NativeCall "Math.log" xs
    DoubleSqrtOp   -> NativeCall "Math.sqrt" xs
    DoubleCosOp    -> NativeCall "Math.cos" xs
    DoubleSinOp    -> NativeCall "Math.sin" xs
    DoubleTanOp    -> NativeCall "Math.tan" xs
    DoubleAcosOp   -> NativeCall "Math.acos" xs
    DoubleAsinOp   -> NativeCall "Math.asin" xs
    DoubleAtanOp   -> NativeCall "Math.atan" xs
    DoubleCoshOp   -> NativeCall "cosh" xs
    DoubleSinhOp   -> NativeCall "sinh" xs
    DoubleTanhOp   -> NativeCall "tanh" xs
    DoubleDecode_2IntOp -> NativeCall "decodeDouble" xs
    DoubleGtOp -> binOp AST.GT
    DoubleGeOp -> binOp GTE
    DoubleEqOp -> binOp Eq
    DoubleNeOp -> binOp Neq
    DoubleLtOp -> binOp AST.LT
    DoubleLeOp -> binOp LTE
    DoubleAddOp -> binOp Add
    DoubleSubOp -> binOp Sub
    DoubleMulOp -> binOp Mul
    DoubleDivOp -> binOp Div
    DoublePowerOp -> call "Math.pow"

    -- Float ops
    FloatExpOp     -> NativeCall "Math.exp" xs
    FloatLogOp     -> NativeCall "Math.log" xs
    FloatSqrtOp    -> NativeCall "Math.sqrt" xs
    FloatCosOp     -> NativeCall "Math.cos" xs
    FloatSinOp     -> NativeCall "Math.sin" xs
    FloatTanOp     -> NativeCall "Math.tan" xs
    FloatAcosOp    -> NativeCall "Math.acos" xs
    FloatAsinOp    -> NativeCall "Math.asin" xs
    FloatAtanOp    -> NativeCall "Math.atan" xs
    FloatCoshOp    -> NativeCall "cosh" xs
    FloatSinhOp    -> NativeCall "sinh" xs
    FloatTanhOp    -> NativeCall "tanh" xs
    FloatDecode_IntOp -> NativeCall "decodeFloat" xs
    FloatGtOp -> binOp AST.GT
    FloatGeOp -> binOp GTE
    FloatEqOp -> binOp Eq
    FloatNeOp -> binOp Neq
    FloatLtOp -> binOp AST.LT
    FloatLeOp -> binOp LTE
    FloatAddOp -> binOp Add
    FloatSubOp -> binOp Sub
    FloatMulOp -> binOp Mul
    FloatDivOp -> binOp Div
    FloatPowerOp -> call "Math.pow"
    
    -- Array ops
    NewArrayOp -> call "newArr"
    SameMutableArrayOp -> Thunk [] $ binOp Eq
    ReadArrayOp -> Array [defTag, defState, Index arr ix]
      where (arr:ix:_) = xs
    WriteArrayOp -> Assign (Index arr ix) rhs
      where (arr:ix:rhs:_) = xs
    SizeofArrayOp -> Index (head xs) (lit "length")
    SizeofMutableArrayOp -> Index (head xs) (lit "length")
    IndexArrayOp -> Array [defTag, Index arr ix]
      where (arr:ix:_) = xs
    UnsafeFreezeArrayOp -> Array [defTag, defState, (head xs)]
    UnsafeThawArrayOp -> Array [defTag, defState, (head xs)]
    -- TODO: copy, clone, freeze, thaw
    
    -- Mutable variables
    NewMutVarOp -> call "nMV"
    ReadMutVarOp -> call "rMV"
    WriteMutVarOp -> call "wMV"
    
    -- OffAddr ops
    ReadOffAddrOp_Char -> Index (xs !! 0) (xs !! 1)
    ReadOffAddrOp_Int8 -> Index (xs !! 0) (xs !! 1)
    ReadOffAddrOp_Word8 -> Index (xs !! 0) (xs !! 1)
    ReadOffAddrOp_WideChar -> Index (xs !! 0) (xs !! 1)
    WriteOffAddrOp_WideChar -> call "wOffAddr"
    WriteOffAddrOp_Int8 -> call "wOffAddr"

    -- ByteArray ops
    NewAlignedPinnedByteArrayOp_Char -> NativeCall "newBA" [xs!!0, xs!!2]
    UnsafeFreezeByteArrayOp -> Array $ [litN 1,xs!!1,xs!!0]
    ByteArrayContents_Char -> head xs

    -- Misc. opts
    -- noDuplicate is only relevant in a threaded environment.
    TouchOp        -> xs !! 1
    RaiseOp        -> call "die"
    RaiseIOOp      -> call "die"
    NoDuplicateOp  -> head xs
    x              -> runtimeError $ "Unsupported PrimOp: " ++ show x
  where
    call f = NativeCall f xs
    binOp bop = let [x, y] = xs in BinOp bop x y
