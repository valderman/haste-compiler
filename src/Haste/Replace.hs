{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Haste.Replace (Replace (..), repNothing, repVars,
                                   repVarsLHS, replace) where
import Haste.AST as AST

-- | Interface for functions that replace a node in the AST with another.
data Replace = Replace {
    repExp :: JSExp -> JSExp,
    repStm :: JSStmt -> JSStmt,
    repAlt :: JSAlt -> JSAlt,
    repVar :: JSVar -> JSVar
  }

-- | Don't replace anything.
repNothing :: Replace
repNothing = Replace id id id id

-- | Replace one set of variables with another.
repVars :: [(JSVar, JSVar)] -> Replace
repVars fromTo = repNothing {
    repVar = \v -> rep v fromTo
  }
  where
    rep v ((from, to):xs) | v == from = to
                          | otherwise = rep v xs
    rep v _                           = v

-- | Replace one set of variables with another, but only on the left hand side
--   of assignments.
repVarsLHS :: [(JSVar, JSVar)] -> Replace
repVarsLHS fromTo = repNothing {
    repStm = repS,
    repExp = repE
  }
  where
    repS (NewVar (AST.Var v) e) = NewVar (AST.Var $ rep v fromTo) e
    repS x                      = x
    repE (Assign (AST.Var v) e) = Assign (AST.Var $ rep v fromTo) e
    repE x                      = x
    
    rep v ((from, to):xs) | v == from = to
                          | otherwise = rep v xs
    rep v _                           = v

class ASTReplace a where
  replace :: Replace -> a -> a

instance ASTReplace a => ASTReplace [a] where
  replace r = map (replace r)

instance ASTReplace JSStmt where
  replace r@(Replace _ rStm _ _) = go
    where
      go = go' . rStm
      go' (Ret ex)          = Ret (replace r ex)
      go' (While e s)       = While (replace r e) (go s)
      go' (Block ss)        = Block (map go ss)
      go' (Case e as)       = Case (replace r e) (replace r as)
      go' (If e th el)      = If (replace r e) (map go th) (map go el)
      go' (NewVar v e)      = NewVar (replace r v) (replace r e)
      go' (NamedFun n as b) = NamedFun n (replace r as) (map go b)
      go' (ExpStmt e)       = ExpStmt (replace r e)
      go' (Continue)        = Continue
      go' (LocalCopy l v b) = LocalCopy (replace r l) (replace r v) (map go b)
      go' _                 = error "Missing case in replace for JSStmt!"

instance ASTReplace JSExp where
  replace r@(Replace rExp _ _ _) = go
    where
      go = go' . rExp
      go' (Call fn as)             = Call (go fn) (map go as)
      go' (FastCall fn as)         = FastCall (go fn) (map go as)
      go' (NativeCall fn as)       = NativeCall fn (map go as)
      go' (NativeMethCall fn m as) = NativeMethCall (go fn) m (map go as)
      go' (Fun as b)               = Fun (replace r as) (replace r b)
      go' (BinOp op a b)           = BinOp op (go a) (go b)
      go' (Neg x)                  = Neg (go x)
      go' (Not x)                  = Not (go x)
      go' (AST.Var v)              = AST.Var (replace r v)
      go' (Lit x)                  = Lit x
      go' (Thunk ss val)           = Thunk (replace r ss) (go val)
      go' (Eval ex)                = Eval (go ex)
      go' (Array exs)              = Array (map go exs)
      go' (Assign lhs rhs)         = Assign (go lhs) (go rhs)
      go' (Index val ix)           = Index (go val) (go ix)
      go' (IfExp cond th el)       = IfExp (go cond) (go th) (go el)
      go' (DataCon t stricts)      = DataCon (go t) stricts
      go' (Null)                   = Null
      go' x                        = error $
                                       "Missing case in replace for JSExp:\n"
                                       ++ show x

instance ASTReplace JSAlt where
  replace r@(Replace _ _ rAlt _) = go
    where
      go = go' . rAlt
      go' (Cond expr branch) = Cond (replace r expr) (replace r branch)
      go' (Def branch)       = Def (replace r branch)
      go' _                  = error "Missing case in replace for JSAlt"

instance ASTReplace JSVar where
  replace r@(Replace _ _ _ rVar) = rVar
