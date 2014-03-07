{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, GADTs, OverloadedStrings #-}
module Data.JSTarget.Print () where
import Prelude hiding (LT, GT)
import Data.JSTarget.AST
import Data.JSTarget.Op
import Data.JSTarget.PP as PP
import Blaze.ByteString.Builder.Char.Utf8
import Data.Monoid
import Control.Monad
import Data.Char
import Numeric (showHex)

instance Pretty Var where
  pp (Foreign name) =
    put $ fromString name
  pp (Internal name comment) = do
    pp name
    doComment <- getOpt nameComments
    when (doComment && not (null comment)) $
      put $ "/* " <> fromString comment <> " */"

instance Pretty Name where
  pp name = finalNameFor name >>= put . buildFinalName

instance Pretty LHS where
  pp (NewVar _ v) = "var " .+. pp v
  pp (LhsExp ex)  = pp ex

instance Pretty Lit where
  pp (LNum d)  = put d
  pp (LStr s)  = "\"" .+. put (fixQuotes s) .+. "\""
    where
      fixQuotes ('\\':xs) = "\\\\" ++ fixQuotes xs
      fixQuotes ('"':xs)  = '\\':'"'  : fixQuotes xs
      fixQuotes ('\'':xs) = '\\':'\'' : fixQuotes xs
      fixQuotes ('\r':xs) = '\\':'r'  : fixQuotes xs
      fixQuotes ('\n':xs) = '\\':'n' : fixQuotes xs
      fixQuotes (x:xs)
        | ord x <= 127 = x : fixQuotes xs
        | otherwise    = toHex x ++ fixQuotes xs
      fixQuotes _             = []
  pp (LBool b) = put b
  pp (LInt n)  = put n
  pp (LNull)   = "null"

-- | Generate a Haskell \uXXXX escape sequence for a char if it's >127.
toHex :: Char -> String
toHex c =
  case ord c of
    n | n < 127   -> [c]
      | otherwise -> "\\u" ++ exactlyFour (showHex (n `rem` 65536) "")

-- | Truncate and pad a string to exactly four characters. '0' is used for padding.
exactlyFour :: String -> String
exactlyFour s =
    pad (4-len) $ drop (len-4) s
  where
    len = length s
    pad 0 cs = cs
    pad n cs = '0' : pad (n-1) cs


-- | Default separator; comma followed by space, if spaces are enabled.
sep :: PP ()
sep = "," .+. sp

instance Pretty Exp where
  pp (Var v) =
    pp v
  pp (Lit l) =
    pp l
  pp (Verbatim s) =
    put s
  pp (Not ex) =
    case neg ex of
      Just ex' -> pp ex'
      _ -> if expPrec (Not ex) > expPrec ex
             then "!(" .+. pp ex .+. ")"
             else "!" .+. pp ex
  pp bop@(BinOp _ _ _) =
    case norm bop of
      BinOp op a b -> opParens op a b
      ex           -> pp ex
  pp (Fun mname args body) = do
      "function" .+. lambdaname .+. "(" .+. ppList sep args .+. "){" .+. newl
      indent $ pp body
      ind .+. "}"
    where
      lambdaname = maybe "" (\n -> " " .+. pp n) mname
  pp (Call _ call f args) = do
      case call of
        Normal   -> "A(" .+. pp f .+. ",[" .+. ppList sep args .+. "])"
        Fast     -> ppCallFun f .+. "(" .+. ppList sep args .+. ")"
        Method m -> pp f .+. put ('.':m) .+. "(" .+. ppList sep args .+. ")"
    where
      ppCallFun fun@(Fun _ _ _) = "(" .+. pp fun .+. ")"
      ppCallFun fun             = pp fun
  pp (Index arr ix) = do
    pp arr .+. "[" .+. pp ix .+. "]"
  pp (Arr exs) = do
    "[" .+. ppList sep exs .+. "]"
  pp (AssignEx l r) = do
    pp l .+. sp .+. "=" .+. sp .+. pp r
  pp (IfEx c th el) = do
    pp c .+. sp .+. "?" .+. sp .+. pp th .+. sp .+. ":" .+. sp .+. pp el

instance Pretty (Var, Exp) where
  pp (v, ex) = pp v .+. sp .+. "=" .+. sp .+. pp ex

-- | Print a series of NewVars at once, to avoid unnecessary "var" keywords.
ppAssigns :: Stm -> PP ()
ppAssigns stm = do
    line $ "var " .+. ppList sep assigns .+. ";"
    pp next
  where
    (assigns, next) = gather [] stm
    gather as (Assign (NewVar _ v) ex nxt) = gather ((v, ex):as) nxt
    gather as nxt                          = (reverse as, nxt)

-- | Returns the final statement in a case branch.
finalStm :: Stm -> PP Stm
finalStm s =
  case s of
    Assign _ _ s'         -> finalStm s'
    Case _ _ _ (Shared l) -> lookupLabel l >>= finalStm
    Forever s'            -> finalStm s'
    _                     -> return s

instance Pretty Stm where
  pp (Case cond def alts (Shared nextRef)) = do
    prettyCase cond def alts
    lookupLabel nextRef >>= pp
  pp (Forever stm) = do
    line "while(1){"
    indent $ pp stm
    line "}"
  pp s@(Assign lhs ex next) = do
    case lhs of
      _ | lhs == blackHole ->
        line (pp ex .+. ";") >> pp next
      NewVar _ _ ->
        ppAssigns s
      LhsExp _ ->
        line (pp lhs .+. sp .+. "=" .+. sp .+. pp ex .+. ";") >> pp next
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

neg :: Exp -> Maybe Exp
neg (BinOp Eq a b)  = Just $ BinOp Neq a b
neg (BinOp Neq a b) = Just $ BinOp Eq a b
neg (BinOp GT a b)  = Just $ BinOp LTE a b
neg (BinOp LT a b)  = Just $ BinOp GTE a b
neg (BinOp GTE a b) = Just $ BinOp LT a b    
neg (BinOp LTE a b) = Just $ BinOp GT a b
neg _               = Nothing

-- | Turn eligible case statements into if statements.
prettyCase :: Exp -> Stm -> [Alt] -> PP ()
prettyCase cond def [(con, branch)] = do
  case (def, branch) of
    (_, NullRet) -> do
      line $ "if(" .+. pp (neg' (test con)) .+. "){"
      indent $ pp def
      line "}"
    (NullRet, _) -> do
      line $ "if(" .+. pp (test con) .+. "){"
      indent $ pp branch
      line "}"
    _ -> do
      line $ "if(" .+. pp (test con) .+."){"
      indent $ pp branch
      line "}else{"
      indent $ pp def
      line "}"
  where
    test (Lit (LBool True))  = cond
    test (Lit (LBool False)) = Not cond
    test (Lit (LNum 0))      = Not cond
    test c                   = BinOp Eq cond c
    neg' c = maybe (Not c) id (neg c)
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
      s <- finalStm branch
      case s of
        Return _ -> return ()
        Cont     -> return ()
        _        -> line "break;";

opParens :: BinOp -> Exp -> Exp -> PP ()
opParens Sub a (BinOp Sub (Lit (LNum 0)) b) =
  opParens Add a b
opParens Sub a (Lit (LNum n)) | n < 0 =
  opParens Add a (Lit (LNum (-n)))
opParens Sub (Lit (LNum 0)) b =
  case b of
    BinOp _ _ _ -> " -(" .+. pp b .+. ")"
    _           -> " -" .+. pp b
opParens op a b = do
  let bparens = case b of
                  Lit (LNum n) | n < 0 -> \x -> "(".+. pp x .+. ")"
                  _                          -> parensR
  parensL a .+. put (fromString $ show op) .+. bparens b
  where
    parensL x = if expPrec x < opPrec op
                  then "(" .+. pp x .+. ")"
                  else pp x
    parensR x = if expPrec x <= opPrec op
                  then "(" .+. pp x .+. ")"
                  else pp x

-- | Normalize an operator expression by shifting parentheses to the left for
--   all associative operators and eliminating comparisons with true/false.
norm :: Exp -> Exp
norm (BinOp op a (BinOp op' b c)) | op == op' && opIsAssoc op =
  norm (BinOp op (BinOp op a b) c)
norm (BinOp Eq a (Lit (LBool True)))   = norm a
norm (BinOp Eq (Lit (LBool True)) b)   = norm b
norm (BinOp Eq a (Lit (LBool False)))  = Not (norm a)
norm (BinOp Eq (Lit (LBool False)) b)  = Not (norm b)
norm (BinOp Neq a (Lit (LBool True)))  = Not (norm a)
norm (BinOp Neq (Lit (LBool True)) b)  = Not (norm b)
norm (BinOp Neq a (Lit (LBool False))) = norm a
norm (BinOp Neq (Lit (LBool False)) b) = norm b
norm e = e
