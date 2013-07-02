{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, GADTs, OverloadedStrings #-}
module Data.JSTarget.Print where
import Data.JSTarget.AST
import Data.JSTarget.Op
import Data.JSTarget.PP as PP
import Data.ByteString.Lazy.Builder
import Data.Monoid
import Control.Monad

instance Pretty Var where
  pp (Foreign name) =
    put $ string7 name
  pp (Internal name comment) = do
    pp name
    doComment <- getOpt nameComments
    when (doComment && not (null comment)) $
      put $ "/* " <> stringUtf8 comment <> " */"

instance Pretty Name where
  pp name = finalNameFor name >>= put . buildFinalName

instance Pretty LHS where
  pp (NewVar _ v) = "var " .+. pp v
  pp (LhsExp ex)  = pp ex

instance Pretty Lit where
  pp (LNum d)  = put d
  pp (LStr s)  = "\"" .+. put (fixQuotes s) .+. "\""
    where
      fixQuotes ('\\':'x':xs) = '\\':'x'  : fixQuotes xs
      fixQuotes ('\\':'u':xs) = '\\':'u'  : fixQuotes xs
      fixQuotes ('\\':xs)     = '\\':'\\' : fixQuotes xs
      fixQuotes ('"':xs)      = '\\':'"'  : fixQuotes xs
      fixQuotes ('\'':xs)     = '\\':'\'' : fixQuotes xs
      fixQuotes ('\n':xs)     = '\\':'n'  : fixQuotes xs
      fixQuotes (x:xs)        = x : fixQuotes xs
      fixQuotes _             = []
  pp (LBool b) = put b
  pp (LInt n)  = put n

instance Pretty Exp where
  pp (Var v) =
    pp v
  pp (Lit l) =
    pp l
  pp (Not ex) =
    if expPrec (Not ex) > expPrec ex 
       then "!(" .+. pp ex .+. ")"
       else "!" .+. pp ex
  pp (BinOp op a b) =
    opParens op a b
  pp (Fun mname args body) = do
      "function" .+. lambdaname .+. "(" .+. ppList "," args .+. "){\n"
      indent $ pp body
      ind .+. "}"
    where
      lambdaname = maybe "" (\n -> " " .+. pp n) mname
  pp (Call _ call f args) = do
    case call of
      Normal   -> "A(" .+. pp f .+. ",[" .+. ppList "," args .+. "])"
      Fast     -> pp f .+. "(" .+. ppList "," args .+. ")"
      Method m -> pp f .+. put ('.':m) .+. "(" .+. ppList "," args .+. ")"
  pp (Index arr ix) = do
    pp arr .+. "[" .+. pp ix .+. "]"
  pp (Arr exs) = do
    "[" .+. ppList "," exs .+. "]"
  pp (AssignEx l r) = do
    pp l .+. "=" .+. pp r
  pp (IfEx c th el) = do
    pp c .+. "?" .+. pp th .+. ":" .+. pp el

instance Pretty Stm where
  pp (Case cond def alts (Shared nextRef)) = do
    prettyCase cond def alts
    lookupLabel nextRef >>= pp
  pp (Forever stm) = do
    line "while(1){"
    indent $ pp stm
    "}"
  pp (Assign lhs ex next) = do
    if lhs == blackHole
      then line $ pp ex .+. ";"
      else line $ pp lhs .+. " = " .+. pp ex .+. ";"
    pp next
  pp (Return ex) = do
    line $ "return " .+. pp ex .+. ";"
  pp (Cont) = do
    line "continue;"
  pp (Jump _) = do
    -- Jumps are essentially fallthroughs which keep track of their
    -- continuation to make analysis and optimization easier.
    return ()
  pp (NullRet) = do
    return ()

-- | Turn eligible case statements into if statements.
prettyCase :: Exp -> Stm -> [Alt] -> PP ()
prettyCase cond def [(con, branch)] = do
  line $ "if(" .+. test con .+."){"
  indent $ pp branch
  line "}else{"
  indent $ pp def
  line "}"
  where
    test (Lit (LBool True))  = pp cond
    test (Lit (LBool False)) = pp $ Not cond
    test (Lit (LNum 0))      = pp $ Not cond
    test _                   = pp cond .+. "==" .+. pp con
prettyCase _ def [] = do
  pp def
prettyCase cond def alts = do
  line $ "switch(" .+. pp cond .+. "){"
  indent $ do
    mapM_ pp alts
    line $ "default:"
    indent $ pp def
  line "}"

instance Pretty Alt where
  pp (con, branch) = do
    line $ "case " .+. pp con .+. ":"
    indent $ do
      pp branch
      "break;"

opParens :: BinOp -> Exp -> Exp -> PP ()
opParens Sub a (BinOp Sub (Lit (LNum 0)) b) =
  opParens Add a b
opParens Sub (Lit (LNum 0)) b =
  "-" .+. pp b
opParens op a b =
  parens a >> put (string7 $ show op) >> parens b
  where
    parens x = if expPrec x < opPrec op
               then "(" .+. pp x .+. ")"
               else pp x
