module CodeGen.Javascript.PrimOps (unOp, binOp) where
import PrimOp
import CodeGen.Javascript.AST as AST

runtimeError :: String -> JSExp
runtimeError s = NativeCall "die" [lit s]

unOp :: PrimOp -> JSExp -> JSExp
unOp op x =
  case op of
    -- Negations
    IntNegOp       -> Neg x
    DoubleNegOp    -> Neg x
    FloatNegOp     -> Neg x
    NotOp          -> Not x -- bitwise
    -- Double ops
    DoubleExpOp    -> NativeCall "Math.exp" [x]
    DoubleLogOp    -> NativeCall "Math.log" [x]
    DoubleSqrtOp   -> NativeCall "Math.sqrt" [x]
    DoubleCosOp    -> NativeCall "Math.cos" [x]
    DoubleSinOp    -> NativeCall "Math.sin" [x]
    DoubleTanOp    -> NativeCall "Math.tan" [x]
    DoubleAcosOp   -> NativeCall "Math.acos" [x]
    DoubleAsinOp   -> NativeCall "Math.asin" [x]
    DoubleAtanOp   -> NativeCall "Math.atan" [x]
    DoubleCoshOp   -> NativeCall "cosh" [x]
    DoubleSinhOp   -> NativeCall "sinh" [x]
    DoubleTanhOp   -> NativeCall "tanh" [x]
    DoubleDecode_2IntOp -> NativeCall "decodeDouble" [x]
    -- Float ops
    FloatNegOp     -> Neg x
    FloatExpOp     -> NativeCall "Math.exp" [x]
    FloatLogOp     -> NativeCall "Math.log" [x]
    FloatSqrtOp    -> NativeCall "Math.sqrt" [x]
    FloatCosOp     -> NativeCall "Math.cos" [x]
    FloatSinOp     -> NativeCall "Math.sin" [x]
    FloatTanOp     -> NativeCall "Math.tan" [x]
    FloatAcosOp    -> NativeCall "Math.acos" [x]
    FloatAsinOp    -> NativeCall "Math.asin" [x]
    FloatAtanOp    -> NativeCall "Math.atan" [x]
    FloatCoshOp    -> NativeCall "cosh" [x]
    FloatSinhOp    -> NativeCall "sinh" [x]
    FloatTanhOp    -> NativeCall "tanh" [x]
    FloatDecode_IntOp -> NativeCall "decodeFloat" [x]
    -- Conversions
    ChrOp          -> NativeCall "String.fromCharCode" [x]
    OrdOp          -> NativeMethCall x "charCodeAt" [lit (0::Double)]
    Word2IntOp     -> x
    Int2WordOp     -> x
    Int2FloatOp    -> x
    Int2DoubleOp   -> x
    Double2IntOp   -> x
    Double2FloatOp -> x
    Float2IntOp    -> x
    Float2DoubleOp -> x
    -- Narrowing ops
    Narrow8IntOp   -> BinOp And x (lit (0xff :: Double))
    Narrow16IntOp  -> BinOp And x (lit (0xffff :: Double))
    Narrow32IntOp  -> BinOp And x (lit (0xffffffff :: Double))
    Narrow8WordOp  -> BinOp And x (lit (0xff :: Double))
    Narrow16WordOp -> BinOp And x (lit (0xffff :: Double))
    Narrow32WordOp -> BinOp And x (lit (0xffffffff :: Double))
    x              -> runtimeError $ "Unsupported PrimOp: " ++ show x

binOp :: PrimOp -> JSExp -> JSExp -> JSExp
binOp op a b =
  op' a b
  where
    call f a b = NativeCall f [a, b]
    op' = case op of
      -- Int ops
      IntAddOp -> BinOp Add
      IntSubOp -> BinOp Sub
      IntMulOp -> BinOp Mul
      IntMulMayOfloOp -> BinOp Mul -- This is correct, but slow!
      IntQuotOp -> call "quot"
      IntRemOp -> BinOp Mod -- Javascript % operator is actually rem, not mod!
      IntAddCOp -> call "addC"
      IntSubCOp -> call "subC"
      ISllOp -> BinOp Shl
      ISraOp -> BinOp ShrA
      ISrlOp -> BinOp ShrL
      IntGtOp -> BinOp AST.GT
      IntGeOp -> BinOp GTE
      IntLtOp -> BinOp AST.LT
      IntLeOp -> BinOp LTE
      IntEqOp -> BinOp Eq
      IntNeOp -> BinOp Neq
      -- Char ops
      CharGtOp -> BinOp AST.GT
      CharGeOp -> BinOp GTE
      CharEqOp -> BinOp Eq
      CharNeOp -> BinOp Neq
      CharLtOp -> BinOp AST.LT
      CharLeOp -> BinOp LTE
      -- Word ops
      WordAddOp -> BinOp Add
      WordSubOp -> BinOp Sub
      WordMulOp -> BinOp Mul
      WordQuotOp -> call "quot"
      WordRemOp -> BinOp Mod
      AndOp -> BinOp BitAnd
      OrOp -> BinOp BitOr
      XorOp -> BinOp BitXor
      SllOp -> BinOp Shl
      SrlOp -> BinOp ShrL
      WordGtOp -> BinOp AST.GT
      WordGeOp -> BinOp GTE
      WordEqOp -> BinOp Eq
      WordNeOp -> BinOp Neq
      WordLtOp -> BinOp AST.LT
      WordLeOp -> BinOp LTE
      -- Double ops
      DoubleGtOp -> BinOp AST.GT
      DoubleGeOp -> BinOp GTE
      DoubleEqOp -> BinOp Eq
      DoubleNeOp -> BinOp Neq
      DoubleLtOp -> BinOp AST.LT
      DoubleLeOp -> BinOp LTE
      DoubleAddOp -> BinOp Add
      DoubleSubOp -> BinOp Sub
      DoubleMulOp -> BinOp Mul
      DoubleDivOp -> BinOp Div
      DoublePowerOp -> call "Math.pow"
      -- Float ops
      FloatGtOp -> BinOp AST.GT
      FloatGeOp -> BinOp GTE
      FloatEqOp -> BinOp Eq
      FloatNeOp -> BinOp Neq
      FloatLtOp -> BinOp AST.LT
      FloatLeOp -> BinOp LTE
      FloatAddOp -> BinOp Add
      FloatSubOp -> BinOp Sub
      FloatMulOp -> BinOp Mul
      FloatDivOp -> BinOp Div
      FloatPowerOp -> call "Math.pow"
      x       -> \_ _ -> runtimeError $ "Unsupported PrimOp: " ++ show x
