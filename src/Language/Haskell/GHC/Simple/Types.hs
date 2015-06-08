-- | Config, input and output types for the simplified GHC API.
module Language.Haskell.GHC.Simple.Types (
    Default (..),
    
    -- Configuration
    StgConfig,
    cfgGhcFlags, cfgUseTargetsFromFlags, cfgUpdateDynFlags, cfgGhcLibDir,
    cfgUseGhcErrorLogger,

    -- Results and errors
    StgModule (..),
    CompResult (..),
    Error (..),
    Warning (..),
    compSuccess
  ) where

-- GHC imports
import GHC
import StgSyn

-- Misc. stuff
import Data.Default

data StgConfig = StgConfig {
    -- | GHC command line flags to control the Haskell to STG compilation
    --   pipeline. Both static and dynamic flags may be set here.
    --   For instance, passing @["-O2", "-DHELLO"]@ here is equivalent to
    --   passing @-O2 -DHELLO@ to the @ghc@ binary.
    --
    --   Note that flags set here are overridden by any changes to 'DynFlags'
    --   performed by 'cfgUpdateDynFlags', and that '--make' mode is always
    --   in effect.
    --
    --   Default: @[]@
    cfgGhcFlags :: [String],

    -- | If file or module names are found among the 'cfgGhcFlags',
    --   should they be used as targets, in addition to any targets given by
    --   other arguments to 'withStg' et al?
    --
    --   Default: @True@
    cfgUseTargetsFromFlags :: Bool,

    -- | Modify the dynamic flags governing the compilation process.
    --   Changes made here take precedence over any flags passed through
    --   'cfgGhcFlags'.
    --
    --   Default: @id@
    cfgUpdateDynFlags :: DynFlags -> DynFlags,

    -- | Use GHC's standard logger to log errors and warnings to the command
    --   line? Errors and warnings are always collected and returned,
    --   regardless of the value of this setting.
    --
    --   Output other than errors and warnings (dumps, etc.) are logged using
    --   the standard logger by default. For finer control over logging
    --   behavior, you should override 'log_action' in 'cfgUpdateDynFlags'.
    --
    --   Default: @False@
    cfgUseGhcErrorLogger :: Bool,
    
    -- | Path to GHC's library directory. If 'Nothing', the library directory
    --   of the system's default GHC compiler will be used.
    --
    --   Default: @Nothing@
    cfgGhcLibDir :: Maybe FilePath
  }

instance Default StgConfig where
  def = StgConfig {
      cfgGhcFlags            = [],
      cfgUseTargetsFromFlags = True,
      cfgUpdateDynFlags      = id,
      cfgUseGhcErrorLogger   = False,
      cfgGhcLibDir           = Nothing
    }

-- | STG bindings and metadata for a single module.
data StgModule = StgModule {
    -- | 'ModSummary' for the module, as given by GHC.
    stgModSummary :: ModSummary,

    -- | String representation of the module's name, not qualified with a
    --   package key.
    --   'ModuleName' representation can be obtained from the module's
    --   'stgModSummary'.
    stgModName :: String,

    -- | String representation of the module's package key.
    --   'PackageKey' representation can be obtained from the module's
    --   'stgModSummary'.
    stgModPackageKey :: String,

    -- | Is this module a compilation target (as opposed to a dependency of
    --   one)?
    stgModIsTarget :: Bool,

    -- | Was the module compiler from a @hs-boot@ file?
    stgModSourceIsHsBoot :: Bool,

    -- | The Haskell source the module was compiled from, if any.
    stgModSourceFile :: Maybe FilePath,

    -- | Interface file corresponding to this module.
    stgModInterfaceFile :: FilePath,
    
    -- | Simplified STG bindings for the module. For more information about
    --   STG and how it relates to Core and proper Haskell, see
    --   <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/StgSynType>.
    stgModBindings :: [StgBinding]
  }

-- | A GHC error message.
data Error = Error {
    -- | Where did the error occur?
    errorSpan      :: SrcSpan,

    -- | Description of the error.
    errorMessage   :: String,

    -- | More verbose description of the error.
    errorExtraInfo :: String
  }

-- | A GHC warning.
data Warning = Warning {
    -- | Where did the warning occur?
    warnSpan    :: SrcSpan,

    -- | What was the warning about?
    warnMessage :: String
  }

-- | Result of a compilation.
data CompResult a
  = Success {
      -- | Result of the compilation.
      compResult   :: a,

      -- | Warnings that occurred during compilation.
      compWarnings :: [Warning],

      -- | Initial 'DynFlags' used by this compilation, collected from 'Config'
      --   data.
      compDynFlags :: DynFlags
    }
  | Failure {
      -- | Errors that occurred during compilation.
      compErrors   :: [Error],

      -- | Warnings that occurred during compilation.
      compWarnings :: [Warning]
    }

-- | Does the given 'CompResult' represent a successful compilation?
compSuccess :: CompResult a -> Bool
compSuccess (Success {}) = True
compSuccess _            = False
