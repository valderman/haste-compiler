{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
-- | Code for generating actual JS from the JS AST.
module CodeGen.Javascript.PrintJS (prettyJS) where
import CodeGen.Javascript.AST
import CodeGen.Javascript.Bag as Bag
import Data.List as List (intersperse, concat)

type Output = String

prettyJS :: [JSStmt] -> Output
prettyJS = List.concat . List.concat . map (toList . pretty 0)

-- | Syntactic sugar for Bag.concat.
(+>) :: Bag a -> Bag a -> Bag a
a +> b = a `Bag.concat` b

-- | Whitespace factory!
indent :: Int -> Bag Output
indent ind = out $ replicate ind ' '

-- | Size of indentation step
step :: Int
step = 4

class PrettyJS a where
  pretty :: Int -> a -> Bag Output

out :: Output -> Bag Output
out = singleton

endl :: Bag Output
endl = out "\n"

instance PrettyJS JSStmt where
  pretty ind (Ret ex) =
    indent ind +> out "return " +> pretty ind ex +> out ";" +> endl
  pretty ind (CallRet f as) =
    pretty ind (Ret (Call f as))
  pretty ind (While ex body) =
    indent ind +> out "while(" +> pretty ind ex +> out ")" +> pretty ind body
  pretty ind (Block stmts) =
    indent ind +> out "{" +> endl +> stmts' +> indent ind +> out "}" +> endl
    where
      stmts' = catLst $ map (pretty $ ind+step) stmts
  pretty ind (Case ex as) =
    indent ind +> out "switch(C(" +> pretty ind ex +> out ")){" +> endl +>
      catLst (map (pretty $ ind+step) as) +> indent ind +> out "}" +> endl
  pretty ind (Assign lhs rhs) =
    indent ind +> pretty ind lhs +> out " = " +> pretty ind rhs +> out ";"+>endl
  pretty ind (NamedFun n as body) =
    indent ind +>
      out "function " +> out n +> out "(" +> prettyList ind "," as +> out "){" +>
        endl +> prettyList (ind+step) "" body +> indent ind +> out "}" +> endl

instance PrettyJS JSAlt where
  pretty ind (Cond ex body) =
    indent ind +> out "case " +> pretty ind ex +> out ":" +> endl +>
      prettyList (ind+step) "" body
  pretty ind (Cons con body) =
    indent ind +> out "case \"" +> out con +> out "\":" +> endl +>
      prettyList (ind+step) "" body
  pretty ind (Def body) =
    indent ind +> out "default:" +> endl +>
      prettyList (ind+step) "" body

instance PrettyJS JSExp where
  pretty ind (Call f as) =
    pretty ind f +> out "(" +> prettyList ind "," as +> out ")"
  pretty ind (Fun as body) =
    out "function(" +> prettyList ind "," as +> out "){" +> endl +>
      prettyList (ind+step) "" body +> indent ind +> out "}"
  pretty ind (BinOp op a b) =
    error "Not doing binary ops yet!"
  pretty ind (Neg x) =
    error "Not doing negation yet!"
  pretty ind (Var v) =
    pretty ind v
  pretty ind (Lit l) =
    pretty ind l
  pretty ind (Thunk ss ex) =
    out "T(function(){" +> endl +> prettyList (ind+step) "" ss +>
      pretty (ind+step) (Ret ex) +> indent ind +> out "})"
  pretty ind (Eval ex) =
    out "E(" +> pretty ind ex +> out ")"
  pretty ind (GetDataArg ex n) =
    pretty ind ex +> out "[" +> out (show n) +> out "]"

instance PrettyJS JSVar where
  pretty ind (Arg n) =
    out $ '_':'a':show n
  pretty ind (Strict n) =
    out $ '_':'s':show n
  pretty ind (Lazy n) =
    out $ '_':'l':show n
  pretty ind (NamedLazy n) =
    out n
  pretty ind (NamedStrict n) =
    out n

instance PrettyJS JSLit where
  pretty _ (Num d) = out $ show d
  pretty _ (Str s) = out s
  pretty _ (Chr c) = out [c]

prettyList ind x xs = catLst $ intersperse (out x) (map (pretty ind) xs)
