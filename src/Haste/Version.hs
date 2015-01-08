-- | Contains version information for Haste.
module Haste.Version (
    BootVer (..),
    hasteVersion, intVersion, ghcVersion, bootVersion,
    showBootVersion, parseBootVersion
  ) where
import System.IO.Unsafe
import Control.Shell (shell, run)
import Data.Version
import Config (cProjectVersion)
import Haste.GHCPaths (ghcBinary)
import Text.ParserCombinators.ReadP
import Data.Maybe (listToMaybe)

-- | Current Haste version.
hasteVersion :: Version
hasteVersion = Version [0, 5, 0] []

-- | Current Haste version as an Int. The format of this version number is
--   MAJOR*10 000 + MINOR*100 + MICRO.
--   Version 1.2.3 would thus be represented as 10203.
intVersion :: Int
intVersion = foldl (\a x -> a*100+x) 0 ver
  where Version ver _ = hasteVersion

-- | The version of GHC used to build this binary.
ghcVersion :: Version
ghcVersion =
    fst $ head $ filter (\(_,s) -> null s) parses
  where
    parses = readP_to_S parseVersion . unsafePerformIO $ do
      res <- shell $ run ghcBinary ["--numeric-version"] ""
      case res of
        Right ver -> return $ init ver -- remove trailing newline
        _         -> return cProjectVersion

-- | Haste + GHC version combo.
bootVersion :: BootVer
bootVersion = BootVer hasteVersion ghcVersion

data BootVer = BootVer {
    bootHasteVersion :: !Version,
    bootGhcVersion   :: !Version
  }

showBootVersion :: BootVer -> String
showBootVersion (BootVer ver ghcver) =
  "haste-" ++ showVersion ver ++ "_ghc-" ++ showVersion ghcver

parseBootVersion :: String -> Maybe BootVer
parseBootVersion =
    fmap fst . listToMaybe . filter (\(_,s) -> null s) . parse
  where
    parse = readP_to_S $ do
      _ <- string "haste-"
      hastever <- parseVersion
      _ <- string "_ghc-"
      ghcver <- parseVersion
      return $ BootVer hastever ghcver
