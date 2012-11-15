module CodeGen.Javascript.Config (
  Config (..), AppStart, defConfig, stdJSLibs, startASAP,
  startOnLoadComplete, appName, sysLibPath, hastePath, evalTrampolining,
  eval, fastMultiply, safeMultiply) where
import CodeGen.Javascript.PrettyM (PrettyOpts, compact)
import CodeGen.Javascript.AST
import System.IO.Unsafe (unsafePerformIO)
import System.Directory
import System.FilePath (combine, replaceExtension)
import Control.Applicative
import Paths_haste_compiler (getDataFileName)
import DynFlags

type AppStart = String -> String

stdJSLibs :: [FilePath]
stdJSLibs = unsafePerformIO $ mapM getDataFileName [
    "rts.js", "stdlib.js", "MVar.js", "StableName.js"
  ]

-- | Name of the application; decides which directories to keep app specific
--   data in.
appName :: String
appName = "haste"

append :: FilePath -> FilePath -> FilePath
append = flip combine

-- | Execute the program as soon as it's loaded into memory.
--   Evaluate the result of applying main, as we might get a thunk back if
--   we're doing TCE. This is so cheap, small and non-intrusive we might
--   as well always do it this way, to simplify the config a bit.
startASAP :: AppStart
startASAP mainSym =
  "E(E(" ++ mainSym ++ ")(0));"

-- | Execute the program when the document has finished loading.
startOnLoadComplete :: AppStart
startOnLoadComplete mainSym =
  "window.onload = function() {" ++ startASAP mainSym ++ "};"

hastePath :: FilePath
hastePath = unsafePerformIO $ getAppUserDataDirectory appName

sysLibPath :: FilePath
sysLibPath = append "lib" hastePath

-- | Int op wrapper for strictly 32 bit (|0).
strictly32Bits :: JSExp -> JSExp
strictly32Bits = flip (BinOp BitOr) (litN 0)

-- | Safe Int multiplication.
safeMultiply :: JSExp -> JSExp -> JSExp
safeMultiply a b = NativeCall "imul" [a, b]

-- | Fast but unsafe Int multiplication.
fastMultiply :: JSExp -> JSExp -> JSExp
fastMultiply = BinOp Mul

-- | Trampolining eval version.
evalTrampolining :: FilePath
evalTrampolining = unsafePerformIO $ getDataFileName "eval-trampoline.js"

-- | Non-trampolining eval version.
eval :: FilePath
eval = unsafePerformIO $ getDataFileName "eval.js"

-- | Compiler configuration.
data Config = Config {
    -- | Runtime files to dump into the JS blob.
    rtsLibs :: [FilePath],
    -- | Which eval version to use. (Will be trampolining or normal.)
    evalLib :: FilePath,
    -- | Path to directory where system libraries are located.
    libPath :: FilePath,
    -- | Write all modules to this path.
    targetLibPath :: FilePath,
    -- | A function that takes the main symbol as its input and outputs the
    --   code that starts the program.
    appStart :: AppStart,
    -- | Options to the pretty printer.
    ppOpts :: PrettyOpts,
    -- | A function that takes the name of the a target as its input and
    --   outputs the name of the file its JS blob should be written to.
    outFile :: String -> String,
    -- | Link the program?
    performLink :: Bool,
    -- | A function to call on each Int arithmetic primop.
    wrapIntMath :: JSExp -> JSExp,
    -- | Operation to use for Int multiplication.
    multiplyIntOp :: JSExp -> JSExp -> JSExp,
    -- | Be verbose about warnings, etc.?
    verbose :: Bool,
    -- | Perform tail call elimination?
    doTCE :: Bool,
    -- | Run the entire thing through Google Closure when done?
    useGoogleClosure :: Maybe FilePath,
    -- | Any external Javascript to link into the JS bundle.
    jsExternals :: [FilePath],
    -- | Dynamic flags used for compilation.
    dynFlags :: DynFlags
  }

-- | Default compiler configuration.
defConfig :: Config
defConfig = Config {
    rtsLibs          = stdJSLibs,
    evalLib          = eval,
    libPath          = sysLibPath,
    targetLibPath    = ".",
    appStart         = startOnLoadComplete,
    ppOpts           = compact,
    outFile          = flip replaceExtension "js",
    performLink      = True,
    wrapIntMath      = strictly32Bits,
    multiplyIntOp    = safeMultiply,
    verbose          = False,
    doTCE            = False,
    useGoogleClosure = Nothing,
    jsExternals      = [],
    dynFlags         = tracingDynFlags
  }
