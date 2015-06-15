{-# LANGUAGE OverloadedStrings, Rank2Types, PatternGuards #-}
module Haste.Config (
  Config (..), AppStart, def, stdJSLibs, startCustom, fastMultiply,
  safeMultiply, debugLib) where
import Data.JSTarget
import Control.Shell (replaceExtension, (</>))
import Data.ByteString.Builder
import Data.Monoid
import Haste.Environment
import Outputable (Outputable)
import Data.Default
import Data.List (stripPrefix, nub)

type AppStart = Builder -> Builder

stdJSLibs :: [FilePath]
stdJSLibs = map (jsDir </>)  [
    "rts.js", "floatdecode.js", "stdlib.js", "jsstring.js", "endian.js",
    "MVar.js", "StableName.js", "Integer.js", "Int64.js", "md5.js", "array.js",
    "pointers.js", "cheap-unicode.js", "Canvas.js", "Handle.js", "Weak.js",
    "Foreign.js"
  ]

debugLib :: FilePath
debugLib = jsDir </> "debug.js"

-- | Execute the program as soon as it's loaded into memory.
--   Evaluate the result of applying main, as we might get a thunk back if
--   we're doing TCE. This is so cheap, small and non-intrusive we might
--   as well always do it this way, to simplify the config a bit.
startASAP :: AppStart
startASAP mainSym =
  mainSym <> "();"

-- | Launch the application using a custom command.
startCustom :: String -> AppStart
startCustom "onload" = startOnLoadComplete
startCustom "asap"   = startASAP
startCustom "onexec" = startASAP
startCustom str      = insertSym str

-- | Replace the first occurrence of $HASTE_MAIN with Haste's entry point
--   symbol.
insertSym :: String -> AppStart
insertSym [] _                              = stringUtf8 ""
insertSym str sym
  | Just r <- stripPrefix "$HASTE_MAIN" str = sym <> stringUtf8 r
  | (l,r) <- span (/= '$') str              = stringUtf8 l <> insertSym r sym

-- | Execute the program when the document has finished loading.
startOnLoadComplete :: AppStart
startOnLoadComplete mainSym =
  "window.onload = " <> mainSym <> ";"

-- | Int op wrapper for strictly 32 bit (|0).
strictly32Bits :: Exp -> Exp
strictly32Bits = flip (binOp BitOr) (litN 0)

-- | Safe Int multiplication.
safeMultiply :: Exp -> Exp -> Exp
safeMultiply a b = callForeign "imul" [a, b]

-- | Fast but unsafe Int multiplication.
fastMultiply :: Exp -> Exp -> Exp
fastMultiply = binOp Mul

-- | Compiler configuration.
data Config = Config {
    -- | Runtime files to dump into the JS blob.
    rtsLibs :: [FilePath],

    -- | Path to directory where system jsmods are located.
    libPaths :: [FilePath],

    -- | Write all jsmods to this path.
    targetLibPath :: FilePath,

    -- | A function that takes the main symbol as its input and outputs the
    --   code that starts the program.
    appStart :: AppStart,

    -- | Wrap the program in its own namespace?
    wrapProg :: Bool,

    -- | Options to the pretty printer.
    ppOpts :: PPOpts,

    -- | A function that takes the name of the a target as its input and
    --   outputs the name of the file its JS blob should be written to.
    outFile :: Config -> String -> String,

    -- | Link the program?
    performLink :: Bool,

    -- | A function to call on each Int arithmetic primop.
    wrapIntMath :: Exp -> Exp,

    -- | Operation to use for Int multiplication.
    multiplyIntOp :: Exp -> Exp -> Exp,

    -- | Be verbose about warnings, etc.?
    verbose :: Bool,

    -- | Perform optimizations over the whole program at link time?
    wholeProgramOpts :: Bool,

    -- | Allow the possibility that some tail recursion may not be optimized
    --   in order to gain slightly smaller code?
    sloppyTCE :: Bool,

    -- | Turn on run-time tracing of primops?
    tracePrimops :: Bool,

    -- | Run the entire thing through Google Closure when done?
    useGoogleClosure :: Maybe FilePath,

    -- | Extra flags for Google Closure to take?
    useGoogleClosureFlags :: [String],

    -- | Any external Javascript to link into the JS bundle.
    jsExternals :: [FilePath],

    -- | Produce a skeleton HTML file containing the program rather than a
    --   JS file.
    outputHTML :: Bool,

    -- | GHC DynFlags used for STG generation.
    --   Currently only used for printing StgSyn values.
    showOutputable :: forall a. Outputable a => a -> String,

    -- | Which module contains the program's main function?
    --   Defaults to Just ("main", "Main")
    mainMod :: Maybe (String, String),

    -- | Perform optimizations.
    --   Defaults to True.
    optimize :: Bool,

    -- | Emit @"use strict";@ declaration. Does not affect minification, but
    --   *does* affect any external JS.
    --   Defaults to True.
    useStrict :: Bool
  }

-- | Default compiler configuration.
defConfig :: Config
defConfig = Config {
    rtsLibs          = stdJSLibs,
    libPaths         = nub [jsmodUserDir, jsmodSysDir],
    targetLibPath    = ".",
    appStart         = startOnLoadComplete,
    wrapProg         = False,
    ppOpts           = def,
    outFile          = \cfg f -> let ext = if outputHTML cfg
                                             then "html"
                                             else "js"
                                 in replaceExtension f ext,
    performLink      = True,
    wrapIntMath      = strictly32Bits,
    multiplyIntOp    = safeMultiply,
    verbose          = False,
    wholeProgramOpts = False,
    sloppyTCE        = False,
    tracePrimops     = False,
    useGoogleClosure = Nothing,
    useGoogleClosureFlags = [],
    jsExternals      = [],
    outputHTML       = False,
    showOutputable   = const "No showOutputable defined in config!",
    mainMod          = Just ("main", "Main"),
    optimize         = True,
    useStrict        = True
  }

instance Default Config where
  def = defConfig
