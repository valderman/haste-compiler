-- | Contains version information for Haste.
module Haste.Version (
    BootVer (..),
    hasteVersion, intVersion, ghcVersion, bootVersion,
    showBootVersion, parseBootVersion,
    showVersion
  ) where
import Data.Version
import Config (cProjectVersion)
import Text.ParserCombinators.ReadP
import Data.Maybe (listToMaybe)

-- | Current Haste version.
hasteVersion :: Version
hasteVersion = Version [0,5,2] []

-- | Current Haste version as an Int. The format of this version number is
--   MAJOR*10 000 + MINOR*100 + MICRO.
--   Version 1.2.3 would thus be represented as 10203.
intVersion :: Int
intVersion = foldl (\a x -> a*100+x) 0 $ take 3 ver
  where Version ver _ = hasteVersion

-- | The version of GHC used to build this binary.
ghcVersion :: Version
ghcVersion =
    fst $ head $ filter (\(_,s) -> null s) parses
  where
    parses = readP_to_S parseVersion cProjectVersion

-- | Haste + GHC version combo.
bootVersion :: BootVer
bootVersion = BootVer hasteVersion ghcVersion

data BootVer = BootVer {
    bootHasteVersion :: !Version,
    bootGhcVersion   :: !Version
  } deriving (Show, Read)

showBootVersion :: BootVer -> String
showBootVersion (BootVer ver ghcver) =
  "haste-" ++ showVersion ver ++ "-ghc-" ++ showVersion ghcver

parseBootVersion :: String -> Maybe BootVer
parseBootVersion =
    fmap fst . listToMaybe . filter (\(_,s) -> null s) . parse
  where
    parse = readP_to_S $ do
      _ <- string "haste-"
      hastever <- parseVersion
      _ <- string "-ghc-"
      ghcver <- parseVersion
      return $ BootVer hastever ghcver
