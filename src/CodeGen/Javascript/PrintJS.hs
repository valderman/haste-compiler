{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Code for generating actual JS from the JS AST.
module CodeGen.Javascript.PrintJS (prettyJS, pseudo, pretty, compact,
                                   PrettyOpts) where
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
prettyJS :: PrettyJS a => PrettyOpts -> JSVar -> a -> (Output, JSLabel)
prettyJS opts mainSym prog = (concat $ bagToList prog', mainSym')
  where
    ((mainSym',_), prog') = runPretty opts $ do
      emit prog
      genUnique mainSym

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
        getName <- extName <$> ask
        bindName' <- getName bindName
        previous >> emit (NamedFun bindName' args body)      
      insTopDef previous bindName expr =
        previous >> emit (NewVar (Var bindName) expr)

instance PrettyJS JSStmt where
  emit (Ret ex) =
    line $ out "return " >> emit ex >> out ";"
  emit (CallRet f as) =
    emit (Ret (Call f as))

  emit (While ex body) = do
    indent $ out "while(" >> emit ex >> out ")"
    emit body

  emit (Block stmts) = do
    line $ out "{" >> endl
    indentFurther $ do
      mapM_ emit stmts
    line $ out "}" >> endl
  
  emit (Case ex alts) = do
    line $ out "switch(C(" >> emit ex >> out ")){"
    indentFurther $ mapM_ emit alts
    line $ out "}"

  emit (If ex thenDo elseDo) = do
    line $ out "if(C(" >> emit ex >> out ")){"
    indentFurther $ emit thenDo
    line $ out "}else{"
    indentFurther $ emit elseDo
    line $ out "}"

  emit (NewVar lhs rhs) = do
    line $ out "var " >> emit lhs >> out " = " >> emit rhs >> out ";"

  emit (NamedFun n as body) = do
    line $ out "function " >> out n >> out "(" >> emitList "," as >> out "){"
    indentFurther $ do
      emitList "" body
    line $ out "}"

  -- Special case for assign statement, as it's so common.
  emit (ExpStmt (Assign lhs rhs)) = do
    line $ emit lhs >> out "=" >> emit rhs >> out ";"

  emit (ExpStmt expr) = do
    line $ emit expr >> out ";"

instance PrettyJS JSAlt where
  emit (Cond ex body) = do
    line $ out "case " >> emit ex >> out ":"
    indentFurther $ do
      emitList "" body
      line $ out "break;"
  
  emit (Def body) = do
    line $ out "default:"
    indentFurther $ do
      emitList "" body

instance PrettyJS JSExp where
  emit (Call f as) =
    out "A(" >> emitList "," [f, Array as] >> out ")"
  emit (FastCall f as) =
    emit f >> out "(" >> emitList "," as >> out ")"
  emit (NativeCall f as) =
    out f >> out "(" >> emitList "," as >> out ")"
  emit (NativeMethCall obj f as) =
    emit obj >> out "." >> out f >> out "(" >> emitList "," as >> out ")"

  emit (Fun as body) = do
    out "function(" >> emitList "," as >> out "){" >> endl
    indentFurther $ do
      emitList "" body
    indent $ out "}"

  -- A nullary data constructor will never be applied to anything, so it's
  -- perfectly safe to generate it as a value right away.
  emit (DataCon tag []) = do
    out "[" >> emit tag >> out "]"
  emit (DataCon tag stricts) = do
    emit $ NativeCall "D" [tag, Array $ map b2n stricts]
    where
      b2n True  = litN 1
      b2n False = litN 0

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
      emit (Ret ex)
    indent $ out "})"

  emit (Eval ex) =
    out "E(" >> emit ex >> out ")"
  
  emit (GetDataArg ex n) =
    emit ex >> out "[" >> out (show n) >> out "]"
  
  emit (Array arr) =
    out "[" >> emitList "," arr >> out "]"
  
  emit (Index arr ix) =
    emit arr >> out "[" >> emit ix >> out "]"

  emit NoOp =
    out "TUT" >> return ()

  emit (IfExp cond thenDo elseDo) =
    emit cond >> out "?" >> emit thenDo >> out ":" >> emit elseDo

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
  emit var =
    case jsname var of
      Foreign n -> out n
      _         -> do
        getName <- extName <$> ask 
        getName var >>= out

-- | Note that string literals INCLUDE double quotes when they come from Core;
--   remember to bring your own if you're creating your own string literals!
instance PrettyJS JSLit where
  emit (Num d) =
    emitNum
    where
      n       = round d :: Int
      n'      = if fromIntegral n == d
                   then show n
                   else show d
      emitNum = if d < 0
                   then out ('(':n') >> out ")"
                   else out n'
  emit (Str s) = out s
  emit (Chr c) = out $ if c == '\'' 
                          then ['"',c,'"']
                          else ['\'',c,'\'']
  emit (Boolean b) = out $ if b then "true" else "false"

emitList :: PrettyJS a => Output -> [a] -> PrettyM ()
emitList between xs = do
  sequence_ $ intersperse (out between) (map emit xs)
