{-# LANGUAGE CPP #-}
-- | Yo dawg we heard you like Haste, so we let yo Haste programs compile yo
--   Haste programs!
--
--   Currently, this API calls the hastec binary. In the future it should
--   probably be the other way around.
module Haste.Compiler (
    module Haste.Compiler.Flags,
    CompileResult (..), HasteOutput (..), HasteInput (..),
    compile
  ) where
import Haste.Compiler.Flags
#ifndef __HASTE__
import Control.Shell
import Haste.Environment
import Data.Maybe
import Data.List (intercalate)
#endif

data CompileResult = Success HasteOutput | Failure String deriving Show
data HasteOutput = OutFile FilePath | OutString String deriving Show
data HasteInput = InFile FilePath | InString String deriving Show

-- | Compile a Haste program using the given compiler flags and source
--   directory.
--   Calling this function from the client will always result in failure.
compile :: CompileFlags -> FilePath -> HasteInput -> IO CompileResult
#ifdef __HASTE__
compile _ _ _ = return $ Failure "Haste can only compile programs server-side."
#else
compile cf dir inp = do
  res <- shell $ do
    curdir <- pwd
    inTempDirectory $ do
      f <- case inp of
        InFile f   -> return $ if isRelative f then incdir curdir </> f else f
        InString s -> file "Main.hs" s >>= \() -> return "Main.hs"
      (f, o, e) <- genericRun hasteBinary (f : idir curdir : mkFlags cf) ""
      if not f
        then do
          return $ Failure e
        else do
          case cfTarget cf of
            TargetFile f -> do
              return $ Success $ OutFile f
            TargetString -> do
              (Success . OutString) `fmap` file "haste.out"
  case res of
    Right res ->
      return res
    Left e ->
      return $ Failure $ "Run-time failure during compilation: " ++ e
  where
    incdir cur = if isRelative dir then cur </> dir else dir
    idir cur = "-i" ++ incdir cur

-- | Turn flags into command line argument.
mkFlags :: CompileFlags -> [String]
mkFlags cf = catMaybes [
    case cfOptimize cf of
      None         -> Just "-O0"
      Basic        -> Nothing
      WholeProgram -> Just "--opt-whole-program",
    case cfStart cf of
      ASAP     -> Just "--start=asap"
      OnLoad   -> Nothing
      Custom s -> Just $ "--start=" ++ s,
    case cfTarget cf of
      TargetFile fp -> Just $ "--out=" ++ fp
      TargetString  -> Just $ "--out=haste.out",
    when cfDebug        "--debug",
    when cfMinify       "--opt-google-closure",
    when cfFullUnicode  "--full-unicode",
    when cfOwnNamespace "--separate-namespace",
    when (not . null . cfJSFiles) ("--with-js=" ++ jsFileList)
  ]
  where
    jsFileList = intercalate "," $ cfJSFiles cf
    when opt arg = if opt cf then Just arg else Nothing
#endif
