module Haste.Opts (hasteOpts, helpHeader) where
import System.Console.GetOpt
import Haste.Config
import Haste.Environment
import Data.JSTarget.PP (PPOpts, withExtAnnotation, withAnnotations, withPretty)
import Data.List
import Control.Shell ((</>))

-- | Haste's command line options. The boolean indicates whether we're running
--   unbooted or not.
hasteOpts :: [OptDescr (Config -> Config)]
hasteOpts = [
    Option "" ["annotate-externals"]
           (NoArg $ \cfg -> cfg {ppOpts = withExtAnnotation (ppOpts cfg)}) $
           "Annotate all JavaScript-native symbols and inline JavaScript " ++
           "in generated code with /* EXTERNAL */.",
    Option "" ["annotate-symbols"]
           (NoArg $ \cfg -> cfg {ppOpts = withAnnotations (ppOpts cfg)}) $
           "Annotate all non-external, non-local symbols with their " ++
           "qualified Haskell names.",
    Option "" ["debug"]
           (NoArg $ \cfg -> cfg {ppOpts = debugPPOpts (ppOpts cfg)}) $
           "Output annotated, pretty-printed JavaScript code. Equivalent to " ++
           "--annotate externals --annotate-symbols --pretty-print.",
    Option "" ["ddisable-js-opts"]
           (NoArg $ \cfg -> cfg {optimize = False}) $
           "Don't perform any optimizations on the JavaScript at all. " ++
           "This notably includes tail call optimization.",
    Option "" ["dtrace-primops"]
           (NoArg $ \cfg -> cfg {tracePrimops = True,
                                 rtsLibs = debugLib : rtsLibs cfg}) $
           "Trace primops. Not really useful unless Haste was booted with " ++
           "primop tracing enabled.",
    Option "" ["dont-link"]
           (NoArg $ \cfg -> cfg {performLink = False}) $
           "Don't link generated .jsmod files into a .js blob.",
    Option "" ["full-unicode"]
           (NoArg fullUnicode) $
           "Enable full generalCategory Unicode support. " ++
           "May bloat output by upwards of 150 KB.",
    Option "?" ["help"]
           (NoArg id) $
           "Display this message.",
    Option "" ["link-jslib"]
           (OptArg (\file cfg -> cfg {linkJSLib = True,
                                      linkJSLibFile = file}) "FILE") $
           "Create a jslib file instead of an executable.",
    Option "" ["no-use-strict"]
           (NoArg $ \cfg -> cfg {useStrict = False}) $
           "Do not emit '\"use strict\";' declaration. Does not affect " ++
           "minifier behavior, but *does* affect any external JavaScript " ++
           "included using --with-js.",
    Option "" ["onexec"]
           (NoArg $ \cfg -> cfg {appStart = startCustom "onexec"}) $
           "Launch application immediately when the JavaScript file is " ++
           "loaded. Shorthand for --start=onexec.",
    Option "" ["onload"]
           (NoArg $ \cfg -> cfg {appStart = startCustom "onload"}) $
           "Launch application on window.onload. " ++
           "Shorthand for --start=onload.",
    Option "" ["opt-all"]
           (NoArg optAllSafe) $
           "Enable all safe optimizations. Equivalent to --opt-minify " ++
           "--opt-whole-program.",
    Option "" ["opt-unsafe"]
           (NoArg optAllUnsafe) $
           "Enable all optimizations, safe and unsafe. Equivalent to " ++
           "--opt-all --opt-unsafe-ints",
    Option "" ["opt-minify"]
           (NoArg updateClosureCfg) $
           "Minify JavaScript output using Google Closure compiler.",
    Option "" ["opt-minify-flag"]
           (ReqArg updateClosureFlags "FLAG") $
           "Pass a flag to Closure. " ++
           "To minify programs in strict mode, use " ++
           "--opt-minify-flag='--language_in=ECMASCRIPT5_STRICT'",
    Option "" ["opt-unsafe-ints"]
           (NoArg unsafeMath) $
           "Enable unsafe Int arithmetic. Implies --opt-unsafe-mult " ++
           "--opt-vague-ints",
    Option "" ["opt-unsafe-mult"]
           (NoArg unsafeMul) $
           "Use JavaScript's built-in multiplication operator for "
           ++ "fixed precision integer multiplication. This may speed "
           ++ "up Int multiplication by a factor of at least four, "
           ++ "but may give incorrect results when the product "
           ++ "falls outside the interval [-2^52, 2^52]. In browsers "
           ++ "which support Math.imul, this optimization will likely be "
           ++ "slower than the default.",
    Option "" ["opt-vague-ints"]
           (NoArg vagueInts) $
           "Int math has 53 bits of precision, but gives incorrect "
           ++ "results rather than properly wrapping around when "
           ++ "those 53 bits are exceeded. Bitwise operations still "
           ++ "only work on the lowest 32 bits.",
    Option "" ["opt-whole-program"]
           (NoArg enableWholeProgramOpts) $
           "Perform optimizations over the whole program during linking. " ++
           "May significantly increase link time.",
    Option "" ["overwrite-scrutinees"]
           (NoArg $ \cfg -> cfg {overwriteScrutinees = True}) $
           "Overwrite scrutinees when evaluated rather than allocating " ++
           "a new local for the evaluated value. This is largely experimental.",
    Option "o" ["out"]
           (ReqArg (\f cfg -> cfg {outFile = \_ _ -> f}) "FILE") $
           "Write JavaScript output to FILE.",
    Option "" ["outdir"]
           (ReqArg (\d cfg -> cfg {targetLibPath = d}) "DIR") $
           "Write intermediate files to DIR.",
    Option "" ["output-html"]
           (NoArg $ \cfg -> cfg {outputHTML = True}) $
           "Write the JavaScript output to an HTML file together with a " ++
           "minimal HTML skeleton.",
    Option "" ["own-namespace"]
           (NoArg $ \cfg -> cfg {wrapProg = True}) $
           "Wrap the whole program in a closure to avoid polluting the " ++
           "global namespace. Incurs a performance hit, and makes " ++
           "minification slightly less effective.",
    Option "" ["pretty-print"]
           (NoArg $ \cfg -> cfg {ppOpts = withPretty (ppOpts cfg)}) $
           "Pretty-print JavaScript output.",
    Option "" ["start"]
           (ReqArg (\start cfg -> cfg {appStart = startCustom start})
                   "CODE") $
           "Specify custom start code. '$HASTE_MAIN' will be replaced with " ++
           "the application's main function. For instance, " ++
           "--start='$(\"foo\").onclick($HASTE_MAIN);' " ++
           "will use jQuery to launch the application whenever the element " ++
           "with the id \"foo\" is clicked.",
    Option "" ["output-jsflow"]
           (NoArg enableJSFlow) $
           "Output code for use with the JSFlow interpreter. Note that " ++
           "this may leave your code crippled, since JSFlow doesn't " ++
           "all of Haste's needs.",
    Option "v" ["verbose"]
           (NoArg $ \cfg -> cfg {verbose = True}) $
           "Display even the most obnoxious warnings and messages.",
    Option "" ["with-js"]
           (ReqArg (\js c -> c {jsExternals = jsExternals c ++ commaBreak js})
                   "FILES") $
           "Link the given comma-separated list of JavaScript files into " ++
           "the final .js file."
  ]

helpHeader :: String
helpHeader = unlines [
    "Usage: hastec [OPTIONS] FILES",
    "",
    "To compile a program with a main function residing in prog.hs:",
    "\n  hastec prog.hs\n",
    "The resulting code may be a bit on the large and/or slow side. For release",
    "builds, using --opt-all is strongly recommended. For debugging, use the --debug",
    "option to increase the readability of the produced code somewhat.",
    "",
    "By default, programs start executing when the window.onload event fires.",
    "This is not always what we want. For instance, when running the test suite",
    "we want the program to start executing immediately upon being loaded into",
    "the interpreter. This behavior can be controlled using the --onload, --onexec",
    "and --start options.",
    "",
    "A summary of the available options are given below."
  ]

-- | Like 'words', except it breaks on commas instead of spaces.
commaBreak :: String -> [String]
commaBreak [] = []
commaBreak s  =
  case span (/= ',') s of
    (w, ws) -> w : commaBreak (drop 1 ws)

-- | Don't wrap Ints.
vagueInts :: Config -> Config
vagueInts cfg = cfg {wrapIntMath = id}

-- | Use fast but unsafe multiplication.
unsafeMul :: Config -> Config
unsafeMul cfg = cfg {multiplyIntOp = fastMultiply}

-- | Enable all unsafe math ops. Remember to update the info text when changing
--   this!
unsafeMath :: Config -> Config
unsafeMath = vagueInts . unsafeMul

-- | Enable all optimizations, both safe and unsafe.
optAllUnsafe :: Config -> Config
optAllUnsafe = optAllSafe . unsafeMath . enableWholeProgramOpts

-- | Enable all safe optimizations.
optAllSafe :: Config -> Config
optAllSafe = enableWholeProgramOpts . updateClosureCfg

-- | Set the path to the Closure compiler.jar to use.
updateClosureCfg :: Config -> Config
updateClosureCfg cfg = cfg {useGoogleClosure = Just closureCompiler}

-- | Add flags for Google Closure to use
updateClosureFlags :: String -> Config -> Config
updateClosureFlags arg cfg = cfg {
  useGoogleClosureFlags = useGoogleClosureFlags cfg ++ commaBreak arg}

-- | Enable optimizations over the entire program.
enableWholeProgramOpts :: Config -> Config
enableWholeProgramOpts cfg = cfg {wholeProgramOpts = True}

-- | Produce output for the JSFlow interpreter.
enableJSFlow :: Config -> Config
enableJSFlow cfg = cfg {
    rtsLibs = [libfile |
               libfile <- rtsLibs cfg,
               not $ any (`isSuffixOf` libfile) jsflowIncompatible] ++
              [jsDir </> "jsflow.js"]
  }
  where
    jsflowIncompatible = ["floatdecode.js", "endian.js"]

-- | Save some space and performance by using degenerate implementations of
--   the Unicode functions.
fullUnicode :: Config -> Config
fullUnicode cfg =
    cfg {rtsLibs = unicode : filter (not . (cheap `isSuffixOf`)) libs}
  where
    libs = rtsLibs cfg
    unicode = jsDir </> "unicode.js"
    cheap = jsDir </> "cheap-unicode.js"

debugPPOpts :: PPOpts -> PPOpts
debugPPOpts = withPretty . withAnnotations . withExtAnnotation
