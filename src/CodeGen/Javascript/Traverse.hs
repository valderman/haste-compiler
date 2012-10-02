{-# LANGUAGE FlexibleInstances, BangPatterns #-}
-- | Monad for traversing the JS AST. The monad is strict in its accumulator
--   and result type.
module CodeGen.Javascript.Traverse (traverse, traverseToMaybe, ASTNode (..),
                                    Traverse (..), ASTType (..)) where
import Data.List
import Control.Monad
import CodeGen.Javascript.AST as AST

data Traverse t a  = Done !t | Next !a | Stop deriving Show

data ASTNode = Exp JSExp | Stmt JSStmt | Alt JSAlt | Var JSVar

instance Monad (Traverse t) where
  return x = Next x

  Done x >>= _ = Done x
  Next x >>= f = f x
  Stop   >>= _ = Stop

done :: t -> Traverse t a
done = Done

stop :: Traverse a b
stop = Stop

traverseToMaybe :: Traverse a a -> Maybe a
traverseToMaybe (Done x) = Just x
traverseToMaybe (Next x) = Just x
traverseToMaybe Stop     = Nothing

traverse :: ASTType a
         => (ASTNode -> acc -> Traverse acc acc)
         -> acc
         -> a
         -> Maybe acc
traverse f acc ast =
  traverseToMaybe $ topDown f ast acc

class ASTType a where
  -- | Traverse the AST in top-down, lexical order.
  --   For instance, the statement while(x) {y; z;} till be traversed as:
  --   while(foo(x, y)) {a; b;}
  --   foo(x, y)
  --   foo
  --   x
  --   y
  --   a;
  --   b;
  topDown :: (ASTNode -> acc -> Traverse acc acc)
          -> a
          -> acc
          -> Traverse acc acc

  wrap :: a -> ASTNode

-- | Equivalent to 'topDown' for an entire list of items.
allTopDown :: ASTType a
           => (ASTNode -> acc -> Traverse acc acc)
           -> [a]
           -> acc
           -> Traverse acc acc
allTopDown f = go
  where
    go (x:xs) acc = f (wrap x) acc >>= go xs
    go _ acc      = return acc

instance ASTType JSStmt where
  wrap = Stmt
  topDown f = go
    where
      goAll (x:xs) acc = f (Stmt x) acc >>= goAll xs
      goAll _ acc      = return acc
      go x acc =
        case x of
          Ret exp            -> x' >>= topDown f exp
          While exp stmt     -> x' >>= topDown f exp >>= go stmt
          Block stmts        -> x' >>= goAll stmts
          Case exp alts      -> x' >>= topDown f exp >>= allTopDown f alts
          If exp ths els     -> x' >>= topDown f exp >>= goAll ths >>= goAll els
          NewVar var exp     -> x' >>= topDown f var >>= topDown f exp
          NamedFun _ as body -> x' >>= allTopDown f as >>= goAll body
          ExpStmt ex         -> x' >>= topDown f ex
          Continue           -> x'
          _                  -> error msg
        where
          x' = f (Stmt x) acc
          msg = "Don't know how to traverse statement:\n" ++ show x


instance ASTType JSExp where
  wrap = Exp
  topDown f = go
    where
      goAll (x:xs) acc = f (Exp x) acc >>= goAll xs
      goAll _ acc      = return acc
      go x acc =
        case x of
          Call fn args           -> x' >>= go fn >>= goAll args
          FastCall fn args       -> x' >>= go fn >>= goAll args
          NativeMethCall fn _ as -> x' >>= go fn >>= goAll as
          Fun args body          -> x' >>= allTopDown f args>>=allTopDown f body
          ConstClosure vs exp    -> x' >>= allTopDown f vs >>= go exp
          BinOp _ a b            -> x' >>= go a >>= go b
          Neg ex                 -> x' >>= go ex
          Not ex                 -> x' >>= go ex
          AST.Var v              -> x' >>= topDown f v
          Lit _                  -> x'
          Thunk stmts value      -> x' >>= allTopDown f stmts >>= go value
          Eval ex                -> x' >>= go ex
          Array exprs            -> x' >>= goAll exprs
          Index val ix           -> x' >>= go val >>= go ix
          IfExp cond th el       -> x' >>= goAll [cond, th, el]
          DataCon tag stricts    -> x' >>= go tag
          _                      -> error msg
        where
          msg = "Don't know how to traverse expression:\n" ++ show x
          x' = f (Exp x) acc

instance ASTType JSAlt where
  wrap = Alt
  topDown f x@(Cond expr branch) acc = 
    f (Alt x) acc >>= topDown f expr >>= allTopDown f branch
  topDown f x@(Def branch) acc =
    f (Alt x) acc >>= allTopDown f branch

instance ASTType JSVar where
  wrap = CodeGen.Javascript.Traverse.Var
  topDown f x acc = f (CodeGen.Javascript.Traverse.Var x) acc
