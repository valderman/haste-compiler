module Haste.Args (parseArgs) where
import System.Console.GetOpt
import Data.List

-- | Parse a list of command line arguments into a config and a list of args
--   for GHC. Non-options are passed directly to GHC.
parseArgs :: [OptDescr (a -> a)]
          -> String
          -> [String]
          -> Either String (a -> a, [String])
parseArgs opts hdr args
  | "--help" `elem` args || "-?" `elem` args =
    Left $ printHelp hdr opts
  | otherwise =
    let (hasteArgs, ghcArgs) = splitOpts opts args
        (cfgs, _, _) = getOpt Permute opts hasteArgs
    in Right (foldl' (flip (.)) id cfgs, ghcArgs)

-- | Split opts into Haste options and others.
splitOpts :: [OptDescr a] -> [String] -> ([String], [String])
splitOpts opts args =
    (hasteArgs ++ outopts, filter (\x -> take 2 x /= "--") others)
  where
    (hasteArgs, others) =
      partition isHasteOpt nonouts

    (outopts, nonouts) =
      findOutputs ([], [], []) args

    -- TODO: this is a horrible hack to generally mangle arguments until
    --       stuff works - replace ASAP with less hacky solution!
    findOutputs (outs, nouts, fin) ("--libinstall" : xs) =
      findOutputs (outs, nouts, "--libinstall" : fin) xs
    findOutputs (outs, nouts, fin) ("-o" : out : xs) =
      findOutputs (("--out=" ++ out) : outs, nouts, fin) xs
    findOutputs (outs, nouts, fin) ("-outputdir" : out : xs) =
      findOutputs (("--outdir=" ++ out) : outs, nouts, fin) xs
    findOutputs (outs, nouts, fin) (x@('-':'o':out) : xs)
      | out == "hi"        = findOutputs (outs, x:nouts, fin) xs
      | out == "suf"       = findOutputs (outs, x:nouts, fin) xs
      | out == "dir"       = findOutputs (outs, x:nouts, fin) xs
      | take 2 out == "pt" = findOutputs (outs, x:nouts, fin) xs
      | otherwise          = findOutputs (("--out="++out):outs,nouts,fin) xs
    findOutputs (outs, nouts, fin) (x:xs) =
      findOutputs (outs, x : nouts, fin) xs
    findOutputs (outs, nouts, fin) _ =
      (reverse outs ++ reverse fin, reverse nouts)

    isHasteOpt opt
      | "-o" `isPrefixOf` opt = False
      | otherwise             = prefixElem opt optnames

    optnames =
      concatMap names opts

    names (Option short long _ _) =
      map (\c -> ['-',c]) short ++ map ("--" ++) long

-- | Does the given list exist as a prefix of some element in the list of
--   lists?
prefixElem :: Eq a => [a] -> [[a]] -> Bool
prefixElem x = or . map (\y -> take (length y) x == y)

printHelp :: String -> [OptDescr a] -> String
printHelp hdr = (hdr ++) . ("\n" ++) . unlines . map helpString

helpString :: OptDescr a -> String
helpString (Option short long opt help) =
    shorts ++ longs ++ "\n" ++ formatHelpMessage 80 help
  where
    (longarg, shortarg) =
      case opt of
        NoArg _    -> ("", "")
        ReqArg _ a -> ('=':a, ' ':a)
        OptArg _ a -> ("[=" ++ a ++ "]", " [" ++ a ++ "]")
    shorts =
      case intercalate ", " (map (\c -> ['-',c]) short) of
        s | null s    -> ""
          | otherwise -> s ++ shortarg ++ ", "
    longs =
      case intercalate ", " (map (\s -> "--" ++ s) long) of
        l | null l    -> ""
          | otherwise -> l ++ longarg

-- | Break lines at n chars, add two spaces before each.
formatHelpMessage :: Int -> String -> String
formatHelpMessage chars help =
    unlines . map ("  " ++) . breakLines 0 [] $ words help
  where
    breakLines len ln (w:ws)
      | length w >= chars-2     = w:unwords (reverse ln):breakLines 0 [] ws
      | len+length w >= chars-2 = unwords (reverse ln):breakLines 0 [] (w:ws)
      | otherwise               = breakLines (len+1+length w) (w:ln) ws
    breakLines _ ln _ =
      [unwords $ reverse ln]
