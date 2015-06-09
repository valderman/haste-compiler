{-# LANGUAGE CPP, PatternGuards #-}
-- | Simplified interface to the GHC API.
module Language.Haskell.GHC.Simple (
    -- Configuration, input and output types
    module Simple.Types,
    Compile,
    StgModule,

    -- GHC re-exports needed to meaningfully process STG and Core.
    module CoreSyn, module StgSyn, module Module,
    module Id, module IdInfo, module Var, module Literal, module DataCon,
    module OccName, module Name,
    module Type, module TysPrim, module TyCon,
    module ForeignCall, module PrimOp,
    module DynFlags, module SrcLoc,
    ModSummary (..), ModGuts (..),
    pkgKeyString, modulePkgKey,
    
    -- Entry points
    compile, compileWith, genericCompile
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
import ErrUtils
import Bag
import SrcLoc
import Outputable

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
import Module

-- Misc. stuff
import Control.Monad
import GHC.Paths (libdir)
import Data.IORef
import Language.Haskell.GHC.Simple.Types as Simple.Types
import Language.Haskell.GHC.Simple.Impl

-- | Compile a list of targets and their dependencies into intermediate code.
--   Uses settings from the the default 'CompConfig'.
compile :: Compile a
        => [String]
        -- ^ List of compilation targets. A target can be either a module
        --   or a file name.
        -> IO (CompResult a)
compile = compileWith def

-- | Compile a list of targets and their dependencies using a custom
--   configuration.
compileWith :: Compile a
            => CompConfig
            -- ^ GHC pipeline configuration.
            -> [String]
            -- ^ List of compilation targets. A target can be either a module
            --   or a file name. Targets may also be read from the specified
            --   'CompConfig', if 'cfgUseTargetsFromFlags' is set.
            -> IO (CompResult a)
compileWith = genericCompile toCode

-- | Compile a list of targets and their dependencies using a custom
--   configuration and compilation function in the 'Ghc' monad. See
--   "Language.Haskell.GHC.Simple.Impl" for more information about building
--   custom compilation functions.
genericCompile :: (DynFlags -> ModSummary -> Ghc a)
               -- ^ Compilation function.
               -> CompConfig
               -- ^ GHC pipeline configuration.
               -> [String]
               -- ^ List of compilation targets. A target can be either a module
               --   or a file name. Targets may also be read from the specified
               --   'CompConfig', if 'cfgUseTargetsFromFlags' is set.
               -> IO (CompResult a)
genericCompile comp cfg files = do
    (flags, _staticwarns) <- parseStaticFlags $ map noLoc (cfgGhcFlags cfg)
    warns <- newIORef []
    runGhc (maybe (Just libdir) Just (cfgGhcLibDir cfg)) $ do
      dfs <- getSessionDynFlags
      (dfs', files2, _dynwarns) <- parseDynamicFlags dfs flags
      let dfs'' = cfgUpdateDynFlags cfg $ dfs' {
                      log_action = logger (log_action dfs') warns
                    }
      _ <- setSessionDynFlags dfs''
      ecode <- genCode (toCompiledModule comp) (files ++ map unLoc files2)
      ws <- liftIO $ readIORef warns
      case ecode of
        Right (finaldfs, code) ->
          return Success {
              compResult = code,
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

-- | Map a compilation function over each 'ModSummary' in the dependency graph
--   of a list of targets.
genCode :: GhcMonad m
         => (DynFlags -> ModSummary -> m a)
         -> [String]
         -> m (Either [Error] (DynFlags, [a]))
genCode comp files = do
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
        code <- mapM (comp dfs . noLog) mss
        return $ Right (dfs, code)
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
