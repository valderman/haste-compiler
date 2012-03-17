module Main where
import GHC
import GHC.Paths (libdir)
import HscMain
import DynFlags hiding (flags)
import TidyPgm
import CorePrep
import CoreSyn
import HscTypes
import GhcMonad
import CodeGen.Javascript
import System.Directory
import System.FilePath (combine)
import Control.Applicative
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  defaultErrorHandler defaultLogAction $ runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    (flags', files, _) <- parseDynamicFlags flags (map noLoc args)
    _ <- setSessionDynFlags flags' {ghcLink = NoLink}
    let files' = map unLoc files

    ts <- mapM (flip guessTarget Nothing) files'
    setTargets ts
    _ <- load LoadAllTargets
    deps <- depanal [] False
    mapM_ (compile flags') deps

compile :: (GhcMonad m) => DynFlags -> ModSummary -> m ()
compile flags mod = do
  (pgm, name) <- prepare flags mod
  targetdir <- getTargetDir []
  let theCode = generate name pgm
  liftIO $ writeModule targetdir theCode

prepare :: (GhcMonad m) => DynFlags -> ModSummary -> m (CoreProgram, ModuleName)
prepare flags theMod = do
  env <- getSession
  let name = moduleName $ ms_mod theMod
  pgm <- parseModule theMod
    >>= typecheckModule
    >>= desugarModule
    >>= liftIO . hscSimplify env . coreModule
    >>= liftIO . tidyProgram env
    >>= prepPgm . fst
  return (pgm, name)
  where
    prepPgm tidy = liftIO $ do
      prepd <- corePrepPgm flags (cg_binds tidy) (cg_tycons tidy)
      return prepd

getTargetDir :: GhcMonad m => [String] -> m FilePath
getTargetDir xs
  | any (== "libinstall") xs =
    liftIO $ append "lib" <$> getAppUserDataDirectory "jsplug"
  | otherwise =
    return "."

append :: FilePath -> FilePath -> FilePath
append = flip combine