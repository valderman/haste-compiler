{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Code for generating actual JS from the JS AST.
module CodeGen.Javascript.PrintJS (prettyJS, pseudo, pretty, compact) where
import CodeGen.Javascript.AST as AST
import qualified Data.Map as M
import qualified Data.Set as S
import Bag
import Data.List as List (intersperse)
import Module (moduleNameString)
import CodeGen.Javascript.PrettyM
import Control.Applicative
import Control.Monad
import Control.Monad.Reader

-- | Pretty print the given JS construct using the specified options.
--   For now, only the presets `pseudo`, `pretty` and `compact` are exported.
prettyJS :: PrettyJS a => PrettyOpts -> a -> Output
prettyJS opts = concat . bagToList . snd . runPretty opts . emit

instance PrettyJS a => PrettyJS [a] where
  emit = emitList ""

instance PrettyJS JSMod where
  emit (JSMod modName dependMap binds) = do
    printHdr <- printHeader <$> ask
    when printHdr $ do
      out "/* Module: " >> out (moduleNameString modName) >> endl
      out "   Dependencies: " >> endl
      prettyDeps
      out "*/" >> endl
    emit binds
    where
      prettyDeps =
        M.foldlWithKey' insDep (return ()) dependMap

      insDep previous fun funDeps = do
        _ <- previous
        indentFurther $ do
          emit fun >> out ": " >> emitList "," (S.toList funDeps) >> endl

instance PrettyJS (M.Map JSVar JSExp) where
  emit = M.foldlWithKey' insTopDef (return ())
    where
      insTopDef :: PrettyM () -> JSVar -> JSExp -> PrettyM ()
      insTopDef previous bindName (Fun args body) = do
        previous
        out "function " >> emit bindName >> out "("
        emitList "," args >> out "){" >> endl
        indentFurther $ do
          indent $ emitList "" body
        indent $ out "}" >> endl
      
      insTopDef previous bindName expr =
        previous >> emit (NewVar (Var bindName) expr)

instance PrettyJS JSStmt where
  emit (Ret ex) =
    indent $ out "return " >> emit ex >> out ";" >> endl
  emit (CallRet f as) =
    emit (Ret (Call f as))

  emit (While ex body) = do
    indent $ out "while(" >> emit ex >> out ")"
    emit body

  emit (Block stmts) = do
    indent $ out "{" >> endl
    indentFurther $ do
      mapM_ emit stmts
    indent $ out "}" >> endl
  
  emit (Case ex alts) = do
    indent $ out "switch(C(" >> emit ex >> out ")){" >> endl
    indentFurther $ mapM_ emit alts
    indent $ out "}" >> endl

  emit (NewVar lhs rhs) = do
    indent $ out "var " >> emit lhs >> out " = " >> emit rhs >> out ";" >> endl

  emit (NamedFun n as body) = do
    indent $ do
      out "function " >> out n >> out "(" >> emitList "," as >> out "){"
      endl
      indentFurther $ do
        emitList "" body
      indent $ out "}" >> endl

instance PrettyJS JSAlt where
  emit (Cond ex body) = do
    indent $ out "case " >> emit ex >> out ":" >> endl
    indentFurther $ do
      emitList "" body
      indent $ out "break;" >> endl
  
  emit (Cons con body) = do
    indent $ out "case " >> out (show con) >> out ":" >> endl
    indentFurther $ do
      emitList "" body
      indent $ out "break;" >> endl

  emit (Def body) = do
    indent $ out "default:" >> endl
    indentFurther $ do
      emitList "" body

instance PrettyJS JSExp where
  emit (Call f as) =
    out "A(" >> emitList "," [f, Array as] >> out ")"
  emit (NativeCall f as) =
    out f >> out "(" >> emitList "," as >> out ")"
  emit (NativeMethCall obj f as) =
    emit obj >> out "." >> out f >> out "(" >> emitList "," as >> out ")"

  emit (Fun as body) = do
    out "function(" >> emitList "," as >> out "){" >> endl
    indentFurther $ do
      emitList "" body
    indent $ out "}"

  emit (BinOp op a b) =
    emitParens op a b

  emit (Neg x) =
    if expPrec x < expPrec (Neg x)
       then out "-(" >> emit x >> out ")"
       else out "-" >> emit x
  
  emit (Not x) =
    if expPrec x < expPrec (Not x)
       then out "~(" >> emit x >> out ")"
       else out "~" >> emit x
  
  emit (Var v) =
    emit v
  emit (Lit l) =
    emit l
  
  emit (Thunk stmts ex) = do
    out "T(function(){" >> endl
    indentFurther $ do
      emitList "" stmts
      indent $ emit (Ret ex)
    indent $ out "})"

  emit (Eval ex) =
    out "E(" >> emit ex >> out ")"
  
  emit (GetDataArg ex n) =
    emit ex >> out "[" >> out (show n) >> out "]"
  
  emit (Array arr) =
    out "[" >> emitList "," arr >> out "]"
  
  emit (Index arr ix) =
    emit arr >> out "[" >> emit ix >> out "]"

  emit (Assign lhs rhs) =
    out "(" >> emit lhs >> out "=" >> emit rhs >> out ")"

-- | Pretty-print operator expressions.
emitParens :: JSOp -> JSExp -> JSExp -> PrettyM ()
emitParens op a b =
  parens a >> emit op >> parens b
  where
    parens x = if expPrec x < opPrec op
                 then out "(" >> emit x >> out ")"
                 else emit x

instance PrettyJS JSOp where
  emit op = out $ case op of
    Add    -> "+"
    Mul    -> "*"
    Sub    -> "-"
    Div    -> "/"
    Mod    -> "%"
    And    -> "&&"
    Or     -> "||"
    Eq     -> "=="
    Neq    -> "!="
    AST.LT -> "<"
    AST.GT -> ">"
    LTE    -> "<="
    GTE    -> ">="
    Shl    -> "<<"
    ShrL   -> ">>>"
    ShrA   -> ">>"
    BitAnd -> "&"
    BitXor -> "^"
    BitOr  -> "|"


instance PrettyJS JSVar where
  emit (Foreign foreignName) =
    out foreignName
  emit theName@(External _ _) = do
    emitName <- extName <$> ask
    emitName theName
  emit (Internal intName) =
    out intName

instance PrettyJS JSLit where
  emit (Num d) = let n = round d :: Int in
    out $ if fromIntegral n == d
            then show n
            else show d
  emit (Str s) = out s
  emit (Chr c) = out [c]

-- emitList :: PrettyJS a => Int -> Output -> [a] -> Bag Output
-- emitList ind x xs = unionManyBags $ intersperse (out x) (map (pretty ind) xs)
emitList :: PrettyJS a => Output -> [a] -> PrettyM ()
emitList between xs = do
  sequence_ $ intersperse (out between) (map emit xs)
