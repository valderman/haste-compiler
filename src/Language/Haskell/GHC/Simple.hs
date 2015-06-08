{-# LANGUAGE CPP, PatternGuards #-}
-- | Simplified interface to the GHC API.
module Language.Haskell.GHC.Simple (
    -- Configuration, input and output types
    module Simple.Types,

    -- GHC re-exports needed to meaningfully process STG
    module StgSyn, module Id, module IdInfo, module Var, module Literal,
    module OccName, module DataCon, module Module, module Name, module Type,
    module TysPrim, module TyCon, module ForeignCall, module PrimOp,
    module DynFlags, module SrcLoc,
    ModSummary (..),
    pkgKeyString, modulePkgKey,
    
    -- Entry points
    compStg, compStg'
  ) where

-- GHC scaffolding
import GHC hiding (Warning)
import GhcMonad (liftIO)
import DynFlags
import HscMain
import HscTypes
import TidyPgm
import CorePrep
import StgSyn
import CoreSyn
import CoreToStg
import SimplStg
#if __GLASGOW_HASKELL__ < 710
import Module hiding (modulePackageId, packageIdString)
import qualified Module as M (modulePackageId, packageIdString)
#else
import Module hiding (modulePackageKey, packageKeyString)
import qualified Module as M (modulePackageKey, packageKeyString)
#endif
import ErrUtils
import Bag
import SrcLoc

-- Convenience re-exports for fiddling with STG
import Name hiding (varName)
import Type
import TysPrim
import TyCon
import Literal
import Var hiding (setIdExported, setIdNotExported, lazySetIdInfo)
import Id
import IdInfo
import OccName hiding (varName)
import DataCon
import ForeignCall
import PrimOp
import Outputable

-- Misc. stuff
import Control.Monad
import GHC.Paths (libdir)
import Data.IORef
import Language.Haskell.GHC.Simple.Types as Simple.Types

#if __GLASGOW_HASKELL__ < 710
modulePkgKey :: Module -> PackageId
modulePkgKey = M.modulePackageId

pkgKeyString :: PackageId -> String
pkgKeyString = M.packageIdString
#else
modulePkgKey :: Module -> PackageId
modulePkgKey = M.modulePackageKey

pkgKeyString :: PackageId -> String
pkgKeyString = M.packageKeyString
#endif

-- | Compile a list of targets and their dependencies into simplified STG.
--   Uses settings from the the default 'StgConfig'.
compStg :: [String]
        -- ^ A list of compilation targets. A target can be either a module
        --   or a file name.
        -> IO (CompResult [StgModule])
compStg = compStg' def

-- | Compile a list of targets and their dependencies into simplified STG.
compStg' :: StgConfig
         -- ^ Haskell to STG pipeline configuration.
         -> [String]
         -- ^ A list of compilation targets. A target can be either a module
         --   or a file name. Targets may also be read from the specified
         --   'StgConfig', if 'cfgUseTargetsFromFlags' is set.
         -> IO (CompResult [StgModule])
compStg' cfg files = do
    (flags, _staticwarns) <- parseStaticFlags $ map noLoc (cfgGhcFlags cfg)
    warns <- newIORef []
    runGhc (maybe (Just libdir) Just (cfgGhcLibDir cfg)) $ do
      dfs <- getSessionDynFlags
      (dfs', files2, _dynwarns) <- parseDynamicFlags dfs flags
      let dfs'' = cfgUpdateDynFlags cfg $ dfs' {
                      log_action = logger (log_action dfs') warns
                    }
      _ <- setSessionDynFlags dfs''
      estgs <- mapStg return (files ++ map unLoc files2)
      ws <- liftIO $ readIORef warns
      case estgs of
        Right (finaldfs, stgs) ->
          return Success {
              compResult = stgs,
              compWarnings = ws,
              compDynFlags = finaldfs
            }
        Left es ->
          return Failure {
              compErrors = es,
              compWarnings = ws
            }
  where
    logger deflog warns dfs severity srcspan style msg
      | cfgUseGhcErrorLogger cfg = do
        logger' deflog warns dfs severity srcspan style msg
        -- Messages other than warnings and errors are already logged by GHC
        -- by default.
        case severity of
          SevWarning -> deflog dfs severity srcspan style msg
          SevError   -> deflog dfs severity srcspan style msg
          _          -> return ()
      | otherwise = do
        logger' deflog warns dfs severity srcspan style msg

    -- Collect warnings and supress errors, since we're collecting those
    -- separately.
    logger' _ w dfs SevWarning srcspan _style msg = do
      liftIO $ atomicModifyIORef' w $ \ws ->
        (Warning srcspan (showSDoc dfs msg) : ws, ())
    logger' _ _ _ SevError _ _ _ = do
      return ()
    logger' output _ dfs sev srcspan style msg = do
      output dfs sev srcspan style msg

-- | Map a compilation function over each 'StgModule' in the dependency graph
--   of a list of targets.
mapStg :: GhcMonad m
       => (StgModule -> IO a)
       -> [String]
       -> m (Either [Error] (DynFlags, [a]))
mapStg generate files = do
    dfs <- getSessionDynFlags
    merrs <- handleSourceError (maybeErrors dfs) $ do
      ts <- mapM (flip guessTarget Nothing) files
      setTargets ts
      loads <- load LoadAllTargets
      return $ if succeeded loads then Nothing else Just []
    case merrs of
      Just errs -> return $ Left errs
      _         -> do
        mss <- depanal [] False
        stgs <- mapM (liftIO . generate <=< toStgModule dfs . noLog) mss
        return $ Right (dfs, stgs)
  where
    -- We logged everything when we did @load@, we don't want to do it twice.
    noLog m =
      m {ms_hspp_opts = (ms_hspp_opts m) {log_action = \_ _ _ _ _ -> return ()}}
    maybeErrors dfs =
      return . Just . map (fromErrMsg dfs) . bagToList . srcErrorMessages

fromErrMsg :: DynFlags -> ErrMsg -> Error
fromErrMsg dfs e = Error {
    errorSpan      = errMsgSpan e,
    errorMessage   = showSDocForUser dfs ctx (errMsgShortDoc e),
    errorExtraInfo = showSDocForUser dfs ctx (errMsgExtraInfo e)
  }
  where
    ctx = errMsgContext e

-- | Compile a 'ModSummary' into an 'StgModule'.
toStgModule :: GhcMonad m => DynFlags -> ModSummary -> m StgModule
toStgModule dfs ms = do
  stg <- toSimplifiedStg dfs ms
  ts <- getTargets
  return $ StgModule {
      stgModSummary        = ms,
      stgModName           = moduleNameString $ ms_mod_name ms,
      stgModPackageKey     = pkgKeyString . modulePkgKey $ ms_mod ms,
      stgModIsTarget       = any (`isTargetOf` ms) ts,
      stgModSourceIsHsBoot = ms_hsc_src ms == HsBootFile,
      stgModSourceFile     = ml_hs_file $ ms_location ms,
      stgModInterfaceFile  = ml_hi_file $ ms_location ms,
      stgModBindings       = stg
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
--   Optimization, etc. decided by 'DynFlags'.
toSimplifiedStg :: GhcMonad m => DynFlags -> ModSummary -> m [StgBinding]
toSimplifiedStg dfs ms =
  toModGuts ms >>= simplify >>= prepare dfs >>= toStgBindings dfs ms

-- | Parse, typecheck and desugar a module.
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
