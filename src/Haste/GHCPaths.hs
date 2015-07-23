-- | Paths to GHC binaries and directories.
module Haste.GHCPaths where
import System.IO.Unsafe
import System.Directory (findExecutable)
import Config (cProjectVersion)
import Data.Maybe (catMaybes)

-- | Path to the GHC binary.
ghcBinary :: FilePath
ghcBinary = unsafePerformIO $ do
  exes <- catMaybes `fmap` mapM findExecutable ["ghc-" ++ cProjectVersion,
                                                "ghc"]
  case exes of
    (exe:_) -> return exe
    _       -> error $  "No appropriate GHC executable in search path!\n"
                     ++ "Are you sure you have GHC " ++ cProjectVersion
                     ++ " installed?"

-- | Path to the GHC binary.
ghcPkgBinary :: FilePath
ghcPkgBinary = unsafePerformIO $ do
  exes <- catMaybes `fmap` mapM findExecutable ["ghc-pkg-" ++ cProjectVersion,
                                                "ghc-pkg"]
  case exes of
    (exe:_) -> return exe
    _       -> error $  "No appropriate ghc-pkg executable in search path!\n"
                     ++ "Are you sure you have GHC " ++ cProjectVersion
                     ++ " installed?"
