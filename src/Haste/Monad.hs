{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Haste.Monad (
    JSGen, genJS, dependOn, getModName, addLocal, getCfg, continue, isolate,
    pushBind, popBind, getCurrentBinding
  ) where
import Control.Monad.State
import Data.JSTarget as J hiding (modName)
import qualified Data.Set as S
import Control.Applicative

data GenState cfg = GenState {
    deps         :: !(S.Set Name),
    locals       :: !(S.Set Name),
    continuation :: !(AST Stm -> AST Stm),
    bindStack    :: [Var],
    modName      :: String,
    config       :: cfg
  }

initialState :: GenState cfg
initialState = GenState {
    deps         = S.empty,
    locals       = S.empty,
    continuation = id,
    bindStack    = [],
    modName      = undefined,
    config       = undefined
  }

newtype JSGen cfg a =
  JSGen (State (GenState cfg) a)
  deriving (Monad, Functor, Applicative)

class Dependency a where
  -- | Add a dependency to the function currently being generated.
  dependOn :: a -> JSGen cfg ()
  -- | Mark a symbol as local, excluding it from the dependency graph.
  addLocal :: a -> JSGen cfg ()

instance Dependency J.Name where
  dependOn v = JSGen $ do
    st <- get
    put st {deps = S.insert v (deps st)}

  addLocal v = JSGen $ do
    st <- get
    put st {locals = S.insert v (locals st)}

instance Dependency J.Var where
  dependOn (Foreign _)    = return ()
  dependOn (Internal n _) = dependOn n
  addLocal (Foreign _)    = return ()
  addLocal (Internal n _) = addLocal n

instance Dependency a => Dependency [a] where
  dependOn = mapM_ dependOn
  addLocal = mapM_ addLocal

instance Dependency a => Dependency (S.Set a) where
  dependOn = dependOn . S.toList
  addLocal = addLocal . S.toList

genJS :: cfg         -- ^ Config to use for code generation.
      -> String      -- ^ Name of the module being compiled.
      -> JSGen cfg a -- ^ The code generation computation.
      -> (a, S.Set J.Name, S.Set J.Name, AST Stm -> AST Stm)
genJS cfg myModName (JSGen gen) =
  case runState gen initialState {modName = myModName, config = cfg} of
    (a, GenState dependencies loc cont _ _ _) ->
      (a, dependencies, loc, cont)

getModName :: JSGen cfg String
getModName = JSGen $ modName <$> get

pushBind :: Var -> JSGen cfg ()
pushBind v = JSGen $ do
  st <- get
  put st {bindStack = v : bindStack st}

popBind :: JSGen cfg ()
popBind = JSGen $ do
  st <- get
  put st {bindStack = tail $ bindStack st}

getCurrentBinding :: JSGen cfg Var
getCurrentBinding = JSGen $ fmap (head . bindStack) get

-- | Add a new continuation onto the current one.
continue :: (AST Stm -> AST Stm) -> JSGen cfg ()
continue cont = JSGen $ do
  st <- get
  put st {continuation = continuation st . cont}

-- | Run a GenJS computation in isolation, returning its results rather than
--   writing them to the output stream. Dependencies and locals are still
--   updated, however.
isolate :: JSGen cfg a -> JSGen cfg (a, AST Stm -> AST Stm)
isolate gen = do
  myMod <- getModName
  cfg <- getCfg
  b <- getCurrentBinding
  let (x, dep, loc, cont) = genJS cfg myMod (pushBind b >> gen)
  dependOn dep
  addLocal loc
  return (x, cont)

getCfg :: JSGen cfg cfg
getCfg = JSGen $ fmap config get
