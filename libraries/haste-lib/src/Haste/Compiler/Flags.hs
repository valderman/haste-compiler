module Haste.Compiler.Flags (
    OptLevel (..), ProgStart (..), HasteTarget (..), MinifyFlag (..),
    ClosureOpt,
    CompileFlags,
    defaultFlags,
    cfOptimize, cfDebug, cfMinify, cfFullUnicode, cfOwnNamespace, cfStart,
    cfJSFiles, cfTarget, cfUseStrict
  ) where
type ClosureOpt = String
data MinifyFlag = DontMinify | Minify (Maybe FilePath) [ClosureOpt]
data OptLevel = None | Basic | WholeProgram
data ProgStart = ASAP | OnLoad | Custom String
data HasteTarget = TargetFile FilePath | TargetString

-- | Flags to pass to the compiler. Defaults are the same as when invoking
--   hastec directly. Consult hastec --help for more detailed descriptions.
data CompileFlags = CompileFlags {
    -- | How much should the given program be optimized?
    --   Default: Basic
    cfOptimize     :: OptLevel,
    -- | Should the output be debuggable? Debug information will be stripped
    --   if the program is minified using Closure.
    --   Default: False
    cfDebug        :: Bool,
    -- | Should the program be minified? This will strip any debug information
    --   from the resulting program.
    --   Default: False
    cfMinify       :: MinifyFlag,
    -- | Use full Unicode compatibility for Data.Char and friends?
    --   Default: False
    cfFullUnicode  :: Bool,
    -- | Put generated code in a separate namespace?
    --   Default: False
    cfOwnNamespace :: Bool,
    -- | How should the program be started?
    --   Default: OnLoad
    cfStart        :: ProgStart,
    -- | Javascript files to include in the final program.
    --   Default: []
    cfJSFiles      :: [FilePath],
    -- | Where to place the compilation output.
    --   Default: TargetString
    cfTarget       :: HasteTarget,
    -- | @'use strict';@?
    --   Default: True
    cfUseStrict    :: Bool
  }

-- | Default compiler flags.
defaultFlags :: CompileFlags
defaultFlags = CompileFlags {
    cfOptimize = Basic,
    cfDebug = False,
    cfMinify = DontMinify,
    cfFullUnicode = False,
    cfOwnNamespace = False,
    cfStart = OnLoad,
    cfJSFiles = [],
    cfTarget = TargetString,
    cfUseStrict = True
  }
