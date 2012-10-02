module CodeGen.Javascript.Replace where
import CodeGen.Javascript.AST as AST
import CodeGen.Javascript.Traverse as T

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

class ASTReplace a where
  replace :: Replace -> a -> a

replAll :: ASTReplace a => Replace -> [a] -> [a]
replAll r = map (replace r)

instance ASTReplace JSStmt where
  replace r@(Replace _ rStm _ _) = go
    where
      go = rStm . go'
      go' (Ret ex)          = Ret (replace r ex)
      go' (While e s)       = While (replace r e) (go s)
      go' (Block ss)        = Block (map go ss)
      go' (Case e as)       = Case (replace r e) (replAll r as)
      go' (If e th el)      = If (replace r e) (map go th) (map go el)
      go' (NewVar v e)      = NewVar (replace r v) (replace r e)
      go' (NamedFun n as b) = NamedFun n (replAll r as) (map go b)
      go' (ExpStmt e)       = ExpStmt (replace r e)
      go' (Continue)        = Continue
      go' _                 = error "Missing case in replace for JSStmt!"

instance ASTReplace JSExp where
  replace r@(Replace rExp _ _ _) = go
    where
      go = rExp . go'
      go' (Call fn as)             = Call (go fn) (map go as)
      go' (FastCall fn as)         = FastCall (go fn) (map go as)
      go' (NativeMethCall fn m as) = NativeMethCall (go fn) m (map go as)
      go' (Fun as b)               = Fun (replAll r as) (replAll r b)
      go' (ConstClosure vs ex)     = ConstClosure (replAll r vs) (go ex)
      go' (BinOp op a b)           = BinOp op (go a) (go b)
      go' (Neg x)                  = Neg (go x)
      go' (Not x)                  = Not (go x)
      go' (AST.Var v)              = AST.Var (replace r v)
      go' (Lit x)                  = Lit x
      go' (Thunk ss val)           = Thunk (replAll r ss) (go val)
      go' (Eval ex)                = Eval (go ex)
      go' (Array exs)              = Array (map go exs)
      go' (Index val ix)           = Index (go val) (go ix)
      go' (IfExp cond th el)       = IfExp (go cond) (go th) (go el)
      go' (DataCon t stricts)      = DataCon (go t) stricts
      go' _                        = error "Missing case in replace for JSExp!"

instance ASTReplace JSAlt where
  replace r@(Replace _ _ rAlt _) = go
    where
      go = rAlt . go'
      go' (Cond expr branch) = Cond (replace r expr) (replAll r branch)
      go' (Def branch)       = Def (replAll r branch)
      go' _                  = error "Missing case in replace for JSAlt!"

instance ASTReplace JSVar where
  replace r@(Replace _ _ _ rVar) = rVar
