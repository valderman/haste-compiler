module Haste.Opts (hasteOpts, helpHeader) where
import System.Console.GetOpt
import Haste.Config
import Haste.Environment
import Data.JSTarget.PP (debugPPOpts)
import Data.List
import Control.Shell ((</>))

-- | Haste's command line options. The boolean indicates whether we're running
--   unbooted or not. When unbooted, --libinstall will install jsmods into the
--   system jsmod dir rather than the user one.
hasteOpts :: Bool -> [OptDescr (Config -> Config)]
hasteOpts unbooted = [
    Option "" ["debug"]
           (NoArg $ \cfg -> cfg {ppOpts = debugPPOpts}) $
           "Output indented, fairly readable code, with all " ++
           "external names included in comments.",
    Option "" ["ddisable-js-opts"]
           (NoArg $ \cfg -> cfg {optimize = False}) $
           "Don't perform any optimizations on the JS at all.",
    Option "" ["dtrace-primops"]
           (NoArg $ \cfg -> cfg {tracePrimops = True,
                                 rtsLibs = debugLib : rtsLibs cfg}) $
           "Trace primops. Not really useful unless Haste was booted with " ++
           "option.",
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
    Option "" ["libinstall"]
           (NoArg $ \cfg -> cfg {targetLibPath = if unbooted
                                                   then jsmodSysDir
                                                   else jsmodUserDir,
                                 performLink = False}) $
           "Install .jsmod files into the user's library. " ++
           "Implies --dont-link.",
    Option "" ["onexec"]
           (NoArg $ \cfg -> cfg {appStart = startCustom "onexec"}) $
           "Launch application immediately when the JS file is loaded. " ++
           "Shorthand for --start=onexec.",
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
           (OptArg updateClosureCfg "PATH") $
           "Minify JS output using Google Closure compiler. " ++
           "Optionally, use the Closure compiler located at PATH.",
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
           "Use Javascript's built-in multiplication operator for "
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
    Option "o" ["out"]
           (ReqArg (\f cfg -> cfg {outFile = \_ _ -> f}) "FILE") $
           "Write JS output to FILE.",
    Option "" ["outdir"]
           (ReqArg (\d cfg -> cfg {targetLibPath = d}) "DIR") $
           "Write intermediate files to DIR.",
    Option "" ["output-html"]
           (NoArg $ \cfg -> cfg {outputHTML = True}) $
           "Write the JS output to an HTML file together with a simple " ++
           "HTML skeleton.",
    Option "" ["start"]
           (ReqArg (\start cfg -> cfg {appStart = startCustom start})
                   "CODE") $
           "Specify custom start code. '$HASTE_MAIN' will be replaced with " ++
           "the application's main function. For instance, " ++
           "--start='$(\"foo\").onclick($HASTE_MAIN);' " ++
           "will use jQuery to launch the application whenever the element " ++
           "with the id \"foo\" is clicked.",
    Option "v" ["verbose"]
           (NoArg $ \cfg -> cfg {verbose = True}) $
           "Display even the most obnoxious warnings and messages.",
    Option "" ["with-js"]
           (ReqArg (\js c -> c {jsExternals = jsExternals c ++ commaBreak js})
                   "FILES") $
           "Link the given comma-separated list of JS files into the final " ++
           "JS bundle."
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
optAllSafe = enableWholeProgramOpts . updateClosureCfg Nothing

-- | Set the path to the Closure compiler.jar to use.
updateClosureCfg :: Maybe FilePath -> Config -> Config
updateClosureCfg (Just fp) cfg =
  cfg {useGoogleClosure = Just fp}
updateClosureCfg _ cfg =
  cfg {useGoogleClosure = Just closureCompiler}

-- | Add flags for Google Closure to use
updateClosureFlags :: String -> Config -> Config
updateClosureFlags arg cfg = cfg {
  useGoogleClosureFlags = useGoogleClosureFlags cfg ++ commaBreak arg}

-- | Enable optimizations over the entire program.
enableWholeProgramOpts :: Config -> Config
enableWholeProgramOpts cfg = cfg {wholeProgramOpts = True}

-- | Save some space and performance by using degenerate implementations of
--   the Unicode functions.
fullUnicode :: Config -> Config
fullUnicode cfg =
    cfg {rtsLibs = unicode : filter (not . (cheap `isSuffixOf`)) libs}
  where
    libs = rtsLibs cfg
    unicode = jsDir </> "unicode.js"
    cheap = jsDir </> "cheap-unicode.js"
