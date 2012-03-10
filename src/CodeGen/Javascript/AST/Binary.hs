-- | Binary instance for the JS AST.
module CodeGen.Javascript.AST.Binary where
import Control.Applicative
import Data.Binary
import Data.Binary.IEEE754
import Module (moduleNameString, mkModuleName)
import CodeGen.Javascript.AST
import Prelude hiding (LT, GT)

instance Binary JSVar where
  put (Foreign lbl) =
    putWord8 0 >> put lbl
  put (External uniq ext) =
    putWord8 1 >> put uniq >> put ext
  put (Internal lbl) =
    putWord8 2 >> put lbl

  get = do
    w <- getWord8
    case w of
      0 -> Foreign  <$> get
      1 -> External <$> get <*> get
      2 -> Internal <$> get

instance Binary JSStmt where
  put (Ret exp) =
    putWord8 0 >> put exp
  put (CallRet f xs) =
    putWord8 1 >> put f >> put xs
  put (While cond body) =
    putWord8 2 >> put cond >> put body
  put (Block stmts) =
    putWord8 3 >> put stmts
  put (Case exp alts) =
    putWord8 4 >> put exp >> put alts
  put (NewVar lhs rhs) =
    putWord8 5 >> put lhs >> put rhs
  put (NamedFun name args body) =
    putWord8 6 >> put name >> put args >> put body

  get = do
    w <- getWord8
    case w of
      0 -> Ret      <$> get
      1 -> CallRet  <$> get <*> get
      2 -> While    <$> get <*> get
      3 -> Block    <$> get
      4 -> Case     <$> get <*> get
      5 -> NewVar   <$> get <*> get
      6 -> NamedFun <$> get <*> get <*> get

instance Binary JSAlt where
  put (Cond exp body) =
    putWord8 0 >> put exp >> put body
  put (Def body) =
    putWord8 1 >> put body
  put (Cons tag body) =
    putWord8 2 >> put tag >> put body

  get = do
    w <- getWord8
    case w of
      0 -> Cond <$> get <*> get
      1 -> Def  <$> get
      2 -> Cons <$> get <*> get

instance Binary JSExp where
  put (Call f xs) =
    putWord8 0 >> put f >> put xs
  put (NativeCall f xs) =
    putWord8 1 >> put f >> put xs
  put (NativeMethCall obj f xs) =
    putWord8 2 >> put obj >> put f >> put xs
  put (Fun args body) =
    putWord8 3 >> put args >> put body
  put (BinOp op a b) =
    putWord8 4 >> put op >> put a >> put b
  put (Neg exp) =
    putWord8 5 >> put exp
  put (Not exp) =
    putWord8 6 >> put exp
  put (Var v) =
    putWord8 7 >> put v
  put (Lit l) =
    putWord8 8 >> put l
  put (Thunk stmts exp) =
    putWord8 9 >> put stmts >> put exp
  put (Eval exp) =
    putWord8 10 >> put exp
  put (GetDataArg exp n) =
    putWord8 11 >> put exp >> put n
  put (Array elems) =
    putWord8 12 >> put elems
  put (Assign lhs rhs) =
    putWord8 13 >> put lhs >> put rhs
  put (Index arr ix) =
    putWord8 14 >> put arr >> put ix

  get = do
    w <- getWord8
    case w of
      0  -> Call           <$> get <*> get
      1  -> NativeCall     <$> get <*> get
      2  -> NativeMethCall <$> get <*> get <*> get
      3  -> Fun            <$> get <*> get
      4  -> BinOp          <$> get <*> get <*> get
      5  -> Neg            <$> get
      6  -> Not            <$> get
      7  -> Var            <$> get
      8  -> Lit            <$> get
      9  -> Thunk          <$> get <*> get
      10 -> Eval           <$> get
      11 -> GetDataArg     <$> get <*> get
      12 -> Array          <$> get
      13 -> Assign         <$> get <*> get
      14 -> Index          <$> get <*> get

instance Binary JSLit where
  put (Num d) =
    putWord8 0 >> put d
  put (Str str) =
    putWord8 1 >> put str
  put (Chr c) =
    putWord8 2 >> put c

  get = do
    w <- getWord8
    case w of
      0 -> Num <$> get
      1 -> Str <$> get
      2 -> Chr <$> get

instance Binary JSOp where
  put = putWord8 . tag
    where
      tag Add    = 0
      tag Mul    = 1
      tag Sub    = 2
      tag Div    = 3
      tag Mod    = 4
      tag And    = 5
      tag Or     = 6
      tag Eq     = 7
      tag Neq    = 8
      tag LT     = 9
      tag GT     = 10
      tag LTE    = 11
      tag GTE    = 12
      tag Shl    = 13
      tag ShrL   = 14
      tag ShrA   = 15
      tag BitAnd = 16
      tag BitOr  = 17
      tag BitXor = 18

  get = do
    w <- getWord8
    return $ case w of
      0  -> Add
      1  -> Mul
      2  -> Sub
      3  -> Div
      4  -> Mod
      5  -> And
      6  -> Or
      7  -> Eq
      8  -> Neq
      9  -> LT
      10 -> GT
      11 -> LTE
      12 -> GTE
      13 -> Shl
      14 -> ShrL
      15 -> ShrA
      16 -> BitAnd
      17 -> BitOr
      18 -> BitXor

instance Binary JSMod where
  put (JSMod name deps code) = do
    put $ moduleNameString name
    put deps
    put code

  get =
    JSMod <$> (mkModuleName <$> get) <*> get <*> get
