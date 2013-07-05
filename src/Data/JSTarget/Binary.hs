{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Binary instances for JSTarget types.
module Data.JSTarget.Binary () where
import Prelude hiding (LT, GT)
import Data.Binary
import Data.Array
import Control.Applicative
import Data.JSTarget.AST
import Data.JSTarget.Op

instance Binary a => Binary (AST a) where
  put (AST x jumps) = put x >> put jumps
  get = AST <$> get <*> get

instance Binary Module where
  put (Module fp name deps defs) = put fp >> put name >> put deps >> put defs
  get = Module <$> get <*> get <*> get <*> get

instance Binary Var where
  put (Foreign str)           = putWord8 0 >> put str
  put (Internal name comment) = putWord8 1 >> put name >> put comment

  get = do
    getWord8 >>= ([Foreign <$> get,Internal <$> get <*> get] !!) . fromIntegral

instance Binary LHS where
  put (NewVar r v) = putWord8 0 >> put r >> put v
  put (LhsExp e)   = putWord8 1 >> put e
  
  get = getWord8 >>= ([NewVar <$>get<*>get, LhsExp <$> get] !!) . fromIntegral

instance Binary Call where
  put Normal     = putWord8 0
  put Fast       = putWord8 1
  put (Method m) = putWord8 2 >> put m
  
  get = getWord8 >>= ([pure Normal,pure Fast,Method <$> get] !!) . fromIntegral

instance Binary Lit where
  put (LNum d)  = putWord8 0 >> put d
  put (LStr s)  = putWord8 1 >> put s
  put (LBool b) = putWord8 2 >> put b
  put (LInt n)  = putWord8 3 >> put n
  put (LNull)   = putWord8 4
  
  get = do
    t <- getWord8
    [LNum <$> get, LStr <$> get, LBool <$> get, LInt <$> get, pure LNull] !!
      fromIntegral t

instance Binary Exp where
  put (Var v)           = putWord8 0 >> put v
  put (Lit l)           = putWord8 1 >> put l
  put (Not ex)          = putWord8 2 >> put ex
  put (BinOp op a b)    = putWord8 3 >> put op >> put a >> put b
  put (Fun nam as body) = putWord8 4 >> put nam >> put as >> put body
  put (Call a c f xs)   = putWord8 5 >> put a >> put c >> put f >> put xs
  put (Index arr ix)    = putWord8 6 >> put arr >> put ix
  put (Arr exs)         = putWord8 7 >> put exs
  put (AssignEx l r)    = putWord8 8 >> put l >> put r
  put (IfEx c th el)    = putWord8 9 >> put c >> put th >> put el
  
  get = do
    tag <- getWord8
    case tag of
      0 -> Var <$> get
      1 -> Lit <$> get
      2 -> Not <$> get
      3 -> BinOp <$> get <*> get <*> get
      4 -> Fun <$> get <*> get <*> get
      5 -> Call <$> get <*> get <*> get <*> get
      6 -> Index <$> get <*> get
      7 -> Arr <$> get
      8 -> AssignEx <$> get <*> get
      9 -> IfEx <$> get <*> get <*> get
      n -> error $ "Bad tag in get :: Get Exp: " ++ show n

instance Binary Stm where
  put (Case e def alts next) =
    putWord8 0 >> put e >> put def >> put alts >> put next
  put (Forever stm) =
    putWord8 1 >> put stm
  put (Assign lhs rhs next) =
    putWord8 2 >> put lhs >> put rhs >> put next
  put (Return ex) =
    putWord8 3 >> put ex
  put (Cont) =
    putWord8 4
  put (Jump j) =
    putWord8 5 >> put j
  put (NullRet) =
    putWord8 6
  
  get = do
    tag <- getWord8
    case tag of
      0 -> Case <$> get <*> get <*> get <*> get
      1 -> Forever <$> get
      2 -> Assign <$> get <*> get <*> get
      3 -> Return <$> get
      4 -> pure Cont
      5 -> Jump <$> get
      6 -> pure NullRet
      n -> error $ "Bad tag in get :: Get Stm: " ++ show n

instance Binary BinOp where
  put Add    = putWord8 0
  put Mul    = putWord8 1
  put Sub    = putWord8 2
  put Div    = putWord8 3
  put Mod    = putWord8 4
  put And    = putWord8 5
  put Or     = putWord8 6
  put Eq     = putWord8 7
  put Neq    = putWord8 8
  put LT     = putWord8 9
  put GT      = putWord8 10
  put LTE    = putWord8 11
  put GTE    = putWord8 12
  put Shl    = putWord8 13
  put ShrL   = putWord8 14
  put ShrA   = putWord8 15
  put BitAnd = putWord8 16
  put BitOr  = putWord8 17
  put BitXor = putWord8 18

  get = (opTbl !) <$> getWord8

instance Binary Name where
  put (Name name owner) = put name >> put owner
  get = Name <$> get <*> get

instance Binary a => Binary (Shared a) where
  put (Shared lbl) = put lbl
  get = Shared <$> get

instance Binary Lbl where
  put (Lbl namespace lbl) = put namespace >> put lbl
  get = Lbl <$> get <*> get

opTbl :: Array Word8 BinOp
opTbl =
    listArray (0, arrLen-1) es
  where
    arrLen = fromIntegral $ length es
    es = [Add, Mul, Sub, Div, Mod, And, Or, Eq, Neq, LT, GT,
          LTE, GTE, Shl, ShrL, ShrA, BitAnd, BitOr, BitXor]
