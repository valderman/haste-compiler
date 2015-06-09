{-# LANGUAGE FlexibleInstances, CPP, PatternGuards #-}
-- | Lower level building blocks for custom code generation.
module Language.Haskell.GHC.Simple.Impl (
    Compile (..), StgModule,
    toSimplifiedStg,
    toSimplifiedCore,
    toModGuts,
    simplify,
    prepare, toStgBindings,
    toCompiledModule,
    modulePkgKey, pkgKeyString
  ) where

-- GHC scaffolding
import GHC hiding (Warning)
import GhcMonad (liftIO)
import HscMain
import HscTypes
import TidyPgm
import CorePrep
import StgSyn
import CoreSyn
import CoreToStg
import SimplStg
#if __GLASGOW_HASKELL__ < 710
import qualified Module as M (modulePackageId, packageIdString)
#else
import qualified Module as M (modulePackageKey, packageKeyString)
#endif

import Control.Monad
import Language.Haskell.GHC.Simple.Types

-- | Any type we can generate intermediate code for.
class Compile a where
  -- | Generate some sort of code from a Haskell module.
  --   Optimizations and other GHC settings, decided by 'DynFlags'.
  toCode :: DynFlags -> ModSummary -> Ghc a

type StgModule = CompiledModule [StgBinding]
instance Compile [StgBinding] where
  toCode = toSimplifiedStg

instance Compile CgGuts where
  toCode = const toSimplifiedCore

instance Compile ModGuts where
  toCode = const toModGuts

-- | Package ID/key of a module.
modulePkgKey :: Module -> PackageId

-- | String representation of a package ID/key.
pkgKeyString :: PackageId -> String

#if __GLASGOW_HASKELL__ < 710
modulePkgKey = M.modulePackageId
pkgKeyString = M.packageIdString
#else
modulePkgKey = M.modulePackageKey
pkgKeyString = M.packageKeyString
#endif

-- | Compile a 'ModSummary' into a module with metadata using a custom
--   compilation function.
toCompiledModule :: GhcMonad m
                 => (DynFlags -> ModSummary -> m a)
                 -> DynFlags
                 -> ModSummary
                 -> m (CompiledModule a)
toCompiledModule comp dfs ms = do
  code <- comp dfs ms
  ts <- getTargets
  return $ CompiledModule {
      modSummary        = ms,
      modName           = moduleNameString $ ms_mod_name ms,
      modPackageKey     = pkgKeyString . modulePkgKey $ ms_mod ms,
      modIsTarget       = any (`isTargetOf` ms) ts,
      modSourceIsHsBoot = ms_hsc_src ms == HsBootFile,
      modSourceFile     = ml_hs_file $ ms_location ms,
      modInterfaceFile  = ml_hi_file $ ms_location ms,
      modCompiledModule = code
    }

-- | Is @t@ the target that corresponds to @ms@?
isTargetOf :: Target -> ModSummary -> Bool
isTargetOf t ms =
  case targetId t of
    TargetModule mn                                -> ms_mod_name ms == mn
    TargetFile fn _
      | ModLocation (Just f) _ _ <- ms_location ms -> f == fn
    _                                              -> False

-- | Compile a 'ModSummary' into a list of simplified 'StgBinding's.
--   See <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/StgSynType>
--   for more information about STG and how it relates to core and Haskell.
toSimplifiedStg :: GhcMonad m => DynFlags -> ModSummary -> m [StgBinding]
toSimplifiedStg dfs ms =
  toSimplifiedCore ms >>= prepare dfs >>= toStgBindings dfs ms

-- | Compile a 'ModSummary' into a 'CgGuts', containing all information about
--   a core module that one could wish for.
toSimplifiedCore :: GhcMonad m => ModSummary -> m CgGuts
toSimplifiedCore = toModGuts >=> simplify

-- | Parse, typecheck and desugar a module. Returned 'ModGuts' structure is not
--   simplified in any way.
toModGuts :: GhcMonad m => ModSummary -> m ModGuts
toModGuts =
  parseModule >=> typecheckModule >=> desugarModule >=> return . coreModule

-- | Simplify a core module for code generation.
simplify :: GhcMonad m => ModGuts -> m CgGuts
simplify mg = do
  env <- getSession
  liftIO $ hscSimplify env mg >>= tidyProgram env >>= return . fst

-- | Prepare a core module for code generation.
prepare :: GhcMonad m => DynFlags -> CgGuts -> m CoreProgram
prepare dfs p = do
  env <- getSession
#if __GLASGOW_HASKELL__ < 710
  liftIO $ corePrepPgm dfs env (cg_binds p) (cg_tycons p)
#else
  liftIO $ corePrepPgm env (ms_location ms) (cg_binds p) (cg_tycons p)
#endif

-- | Turn a core module into a list of simplified STG bindings.
toStgBindings :: GhcMonad m
              => DynFlags -> ModSummary -> CoreProgram -> m [StgBinding]
toStgBindings dfs ms p = liftIO $ do
  stg <- coreToStg dfs (ms_mod ms) p
  fst `fmap` stg2stg dfs (ms_mod ms) stg
