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
  eresult <- shell $ do
    curdir <- pwd
    inTempDirectory $ do
      fil <- case inp of
        InFile f   -> return $ if isRelative f then incdir curdir </> f else f
        InString s -> output "Main.hs" s >> return "Main.hs"
      (f, _, e) <- genericRun hasteBinary (fil : idir curdir : mkFlags cf) ""
      if f /= 0
        then do
          return $ Haste.Compiler.Failure e
        else do
          case cfTarget cf of
            TargetFile tgt -> do
              return $ Haste.Compiler.Success $ OutFile tgt
            TargetString -> do
              (Haste.Compiler.Success . OutString) `fmap` input "haste.out"
  case eresult of
    Right result ->
      return result
    Left e -> return $ Haste.Compiler.Failure $
        "Run-time failure during compilation: " ++ exitString e
  where
    incdir cur = if isRelative dir then cur </> dir else dir
    idir cur = "-i" ++ incdir cur

-- | Turn flags into command line argument.
mkFlags :: CompileFlags -> [String]
mkFlags cf = concat [
    case cfOptimize cf of
      None         -> ["-O0", "--ddisable-js-opts"]
      Basic        -> []
      WholeProgram -> ["--opt-whole-program"],
    case cfStart cf of
      ASAP     -> ["--onexec"]
      OnLoad   -> ["--onload"]
      Custom s -> ["--start=" ++ s],
    case cfTarget cf of
      TargetFile fp -> ["--out=" ++ fp]
      TargetString  -> ["--out=haste.out"],
    case cfMinify cf of
      DontMinify         -> []
      Minify (Just p) fs -> ("--opt-minify="++p) : map appendMinifyFlag fs
      Minify _ fs        -> "--opt-minify" : map appendMinifyFlag fs,
    when cfDebug        "--debug",
    when cfFullUnicode  "--full-unicode",
    when cfOwnNamespace "--separate-namespace",
    when (not . null . cfJSFiles) ("--with-js=" ++ jsFileList),
    when (not . cfUseStrict) "--no-use-strict"
  ]
  where
    appendMinifyFlag f = "--opt-minify-flag=" ++ f
    jsFileList = intercalate "," $ cfJSFiles cf
    when opt arg = if opt cf then [arg] else []
#endif
