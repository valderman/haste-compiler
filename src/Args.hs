module Args (ArgSpec (..), handleArgs) where
import Data.List (partition, stripPrefix)

data ArgSpec a = ArgSpec {
    optName   :: String,
    updateCfg :: a -> [String] -> a,
    info      :: String
  }

-- | Updates a config based on the given list of ArgSpecs and command line
--   arguments. If --help is encountered within the arguments, we stop
--   everything and just return a help message.
handleArgs :: a -> [ArgSpec a] -> [String] -> Either String (a, [String])
handleArgs cfg specs args =
  if elem "--help" cfgargs
     then Left (printHelp specs)
     else Right (foldl (matchSpec specs) cfg (map (drop 2) cfgargs), rest)
  where
    (cfgargs, rest) = partition ((== "--") . take 2) args

matchSpec :: [ArgSpec a] -> a -> String -> a
matchSpec specs cfg arg =
  foldl updCfg cfg matchingSpecs
  where
    updCfg cfg' (spec, args) =
      updateCfg spec cfg' (commaBreak args)
    specArgs =
      map (\spec -> stripPrefix (optName spec) arg) specs
    matchingSpecs =
      map (\(s, Just a) -> (s, a)) $ filter isMatching (zip specs specArgs)
    isMatching (_, Nothing) = False
    isMatching _            = True

-- | Like 'words', except it breaks on commas instead of spaces.
commaBreak :: String -> [String]
commaBreak [] = []
commaBreak s  =
  case span (/= ',') s of
    (w, ws) -> w : commaBreak (drop 1 ws)

printHelp :: [ArgSpec a] -> String
printHelp = unlines . map helpString

helpString :: ArgSpec a -> String
helpString spec =
  "--" ++ hdr ++ "\n"
       ++ formatHelpMessage (info spec)
  where
    hdr =
      case last $ optName spec of
        '=' -> optName spec ++ "<arg>"
        _   -> optName spec

-- | Break lines at 80 chars, add two spaces before each.
formatHelpMessage :: String -> String
formatHelpMessage s =
    unlines $ map ("  " ++) $ breakLines 0 [] ws
  where
    ws = words s
    breakLines len ln (w:ws)
      | len+length w >= 78 = unwords (reverse ln) : breakLines 0 [] (w:ws)
      | otherwise          = breakLines (len+1+length w) (w:ln) ws
    breakLines _ ln _ =
      [unwords $ reverse ln]
