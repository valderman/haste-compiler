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
    ArgSpec { optName = "start=asap",
              updateCfg = \cfg _ -> cfg {appStart = startASAP},
              info = "Start program immediately instead of on document load."},
    ArgSpec { optName = "out=",
              updateCfg = \cfg outfile -> cfg {outFile = const $ head outfile},
              info = "Write the JS blob to <arg>."},
    ArgSpec { optName = "libinstall",
              updateCfg = \cfg _ -> cfg {targetLibPath = sysLibPath,
                                         performLink   = False},
              info = "Install all compiled modules into the user's jsmod "
                     ++ "library\nrather than linking them together into a JS"
                     ++ "blob."},
    ArgSpec { optName = "opt-vague-ints",
              updateCfg = \cfg _ -> cfg {wrapIntMath = id},
              info = "Int math has 53 bits of precision, but gives incorrect "
                     ++ "results rather than properly wrapping around when "
                     ++ "those 53 bits are exceeded. Bitwise operations still "
                     ++ "only work on the lowest 32 bits. This option should "
                     ++ "give a substantial performance boost for Int math "
                     ++ "heavy code."},
    ArgSpec { optName = "opt-tce",
              updateCfg = \cfg _ -> cfg {doTCE = True},
              info = "Perform tail call elimination."},
    ArgSpec { optName = "verbose",
              updateCfg = \cfg _ -> cfg {verbose = True},
              info = "Display even the most obnoxious warnings."}
  ]
