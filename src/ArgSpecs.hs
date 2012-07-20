-- | All of hastec's command line arguments.
module ArgSpecs (argSpecs) where
import Args
import CodeGen.Javascript.Config
import CodeGen.Javascript

argSpecs :: [ArgSpec Config]
argSpecs = [
    ArgSpec { optName = "debug",
              updateCfg = \cfg _ -> cfg {ppOpts  = pretty},
              info = "Output indented, fairly readable code, with all " ++
                     "external names included in comments."},
    ArgSpec { optName = "libinstall",
              updateCfg = \cfg _ -> cfg {targetLibPath = sysLibPath,
                                         performLink   = False},
              info = "Install all compiled modules into the user's jsmod "
                     ++ "library\nrather than linking them together into a JS"
                     ++ "blob."},
    ArgSpec { optName = "opt-all",
              updateCfg = updateClosureCfg,
              info = "Enable all safe optimizations. "
                     ++ "Equivalent to -O2 --opt-google-closure."},
    ArgSpec { optName = "opt-google-closure",
              updateCfg = updateClosureCfg,
              info = "Run the Google Closure compiler on the output. "
                   ++ "Use --opt-google-closure=foo.jar to hint that foo.jar "
                   ++ "is the Closure compiler."},
    ArgSpec { optName = "opt-tce",
              updateCfg = \cfg _ -> cfg {doTCE = True,
                                         evalLib = evalTrampolining},
              info = "Perform tail call elimination."},
    ArgSpec { optName = "opt-vague-ints",
              updateCfg = vagueInts,
              info = "Int math has 53 bits of precision, but gives incorrect "
                     ++ "results rather than properly wrapping around when "
                     ++ "those 53 bits are exceeded. Bitwise operations still "
                     ++ "only work on the lowest 32 bits. This option should "
                     ++ "give a substantial performance boost for Int math "
                     ++ "heavy code."},
    ArgSpec { optName = "opt-unsafe-mult",
              updateCfg = unsafeMul,
              info = "Use Javascript's built-in multiplication operator for "
                     ++ "fixed precision integer multiplication. This speeds "
                     ++ "up Int multiplication by a factor of at least four, "
                     ++ "but may give incorrect results when the product "
                     ++ "falls outside the interval [-2^52, 2^52]."},
    ArgSpec { optName = "out=",
              updateCfg = \cfg outfile -> cfg {outFile = const $ head outfile},
              info = "Write the JS blob to <arg>."},
    ArgSpec { optName = "start=asap",
              updateCfg = \cfg _ -> cfg {appStart = startASAP},
              info = "Start program immediately instead of on document load."},
    ArgSpec { optName = "verbose",
              updateCfg = \cfg _ -> cfg {verbose = True},
              info = "Display even the most obnoxious warnings."},
    ArgSpec { optName = "with-js=",
              updateCfg = \cfg args -> cfg {jsExternals = args},
              info = "Comma-separated list of .js files to include in the "
                   ++ "final JS bundle."}
  ]

-- | Don't wrap Ints.
vagueInts :: Config -> [String] -> Config
vagueInts cfg _ = cfg {wrapIntMath = id}

-- | Use fast but unsafe multiplication.
unsafeMul :: Config -> [String] -> Config
unsafeMul cfg _ = cfg {multiplyIntOp = fastMultiply}

-- | Set the path to the Closure compiler.jar to use.
updateClosureCfg :: Config -> [String] -> Config
updateClosureCfg cfg ['=':arg] =
  cfg {useGoogleClosure = Just arg}
updateClosureCfg cfg _ =
  cfg {useGoogleClosure = Just $ hastePath ++ "/compiler.jar"}
