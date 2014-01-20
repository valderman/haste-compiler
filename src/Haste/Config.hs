{-# LANGUAGE OverloadedStrings #-}
module Haste.Config (
  Config (..), AppStart, defConfig, stdJSLibs, startCustom, fastMultiply,
  safeMultiply, debugLib) where
import Data.JSTarget
import System.FilePath (replaceExtension, (</>))
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Data.Monoid
import Haste.Environment

type AppStart = Builder -> Builder

stdJSLibs :: [FilePath]
stdJSLibs = map (jsDir </>)  [
    "rts.js", "stdlib.js", "MVar.js", "StableName.js", "Integer.js", "Int64.js",
    "md5.js", "array.js", "pointers.js", "cheap-unicode.js", "Canvas.js",
    "Handle.js"
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
startCustom str      = insertSym str

-- | Replace the first occurrence of %% with Haste's entry point symbol.
insertSym :: String -> AppStart
insertSym ('%':'%':str) sym = sym <> fromString str
insertSym (c:str) sym       = fromChar c <> insertSym str sym
insertSym [] _              = fromString ""

-- | Execute the program when the document has finished loading.
startOnLoadComplete :: AppStart
startOnLoadComplete mainSym =
  "window.onload = " <> mainSym <> ";"

-- | Int op wrapper for strictly 32 bit (|0).
strictly32Bits :: AST Exp -> AST Exp
strictly32Bits = flip (binOp BitOr) (litN 0)

-- | Safe Int multiplication.
safeMultiply :: AST Exp -> AST Exp -> AST Exp
safeMultiply a b = callForeign "imul" [a, b]

-- | Fast but unsafe Int multiplication.
fastMultiply :: AST Exp -> AST Exp -> AST Exp
fastMultiply = binOp Mul

-- | Compiler configuration.
data Config = Config {
    -- | Runtime files to dump into the JS blob.
    rtsLibs :: [FilePath],
    -- | Path to directory where system jsmods are located.
    libPath :: FilePath,
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
    outFile :: String -> String,
    -- | Link the program?
    performLink :: Bool,
    -- | A function to call on each Int arithmetic primop.
    wrapIntMath :: AST Exp -> AST Exp,
    -- | Operation to use for Int multiplication.
    multiplyIntOp :: AST Exp -> AST Exp -> AST Exp,
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
    -- | Any external Javascript to link into the JS bundle.
    jsExternals :: [FilePath]
  }

-- | Default compiler configuration.
defConfig :: Config
defConfig = Config {
    rtsLibs          = stdJSLibs,
    libPath          = jsmodDir,
    targetLibPath    = ".",
    appStart         = startOnLoadComplete,
    wrapProg         = False,
    ppOpts           = def,
    outFile          = flip replaceExtension "js",
    performLink      = True,
    wrapIntMath      = strictly32Bits,
    multiplyIntOp    = safeMultiply,
    verbose          = False,
    wholeProgramOpts = False,
    sloppyTCE        = False,
    tracePrimops     = False,
    useGoogleClosure = Nothing,
    jsExternals      = []
  }
