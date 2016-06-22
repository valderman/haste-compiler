{-# LANGUAGE CPP #-}
-- | Read and write JSMods.
module Haste.Module (moduleFilePath, writeModule, readModule) where
import Module (moduleNameSlashes, mkModuleName)
import qualified Data.ByteString.Lazy as B
import Control.Shell
import Haste.AST
import Data.Binary
import Data.List (isSuffixOf)
import qualified Data.ByteString.UTF8 as BS
import qualified Haste.JSLib as JSLib
import qualified System.IO as IO

-- | The file extension to use for modules.
jsmodExt :: Bool -> String
jsmodExt boot = if boot then "jsmod-boot" else "jsmod"

-- | Build the path to the jsmod file corresponding to the given module.
moduleFilePath :: FilePath -- ^ Base path to look for module in.
               -> String   -- ^ Module name; e.g. @Foo.Bar@
               -> Bool     -- ^ Build name for a boot module?
               -> FilePath
moduleFilePath basepath modname boot =
  flip addExtension (jsmodExt boot) $
    basepath </> moduleNameSlashes (mkModuleName modname)

-- | Write a module to file, with the extension specified in `fileExt`.
--   Assuming that fileExt = "jsmod", a module Foo.Bar is written to
--   @basepath/Foo/Bar.jsmod@.
--
--   If any directory in the path where the module is to be written doesn't
--   exist, it gets created.
--
--   Boot modules and "normal" modules get merged at this stage.
writeModule :: FilePath -> Module -> Bool -> IO ()
writeModule basepath m@(Module _ modname _ _) boot =
  fromRight "writeModule" . shell $ do
    mkdir True (takeDirectory path)
    mcompanion <- readMod basepath modstr (not boot)
    m' <- case mcompanion of
            Just companion -> do
              when (isFile bootpath) $ rm bootpath
              return $ merge' m companion
            _              ->
              return m
    liftIO . B.writeFile path $ encode m'
  where
    modstr = BS.toString modname
    path = moduleFilePath basepath modstr boot
    bootpath = moduleFilePath basepath modstr True
    merge' = if boot then merge else flip merge

-- | Read a module from file. If the module is not found at the specified path,
--   libpath/path is tried instead. Returns Nothing is the module is not found
--   on either path.
--
--   This function first looks for an appropriate jslib file. If none is
--   found, then it looks for a standalone jsmod file, possibly with an
--   accompanying jsmod-boot file. If neither is found, it concludes that
--   the module simply does not exist on the given path and returns @Nothing@.
readModule :: FilePath -> String -> String -> IO (Maybe Module)
readModule basepath pkgid modname = fromRight "readModule" . shell $ do
  libfile <- (basepath </>) `fmap` jslibFileName basepath pkgid
  mmlib <- liftIO $ JSLib.readModule libfile modname
  mm <- readMod basepath modname False
  mmboot <- readMod basepath modname True
  case (mmlib, mm, mmboot) of
    (Just m, _, _)          -> return $ Just m
    (_, Just m, Nothing)    -> return $ Just m
    (_, Just m, Just mboot) -> return . Just $ merge mboot m
    _                       -> return Nothing

-- | Get the file name for a given package identifier.
jslibFileName :: FilePath -> String -> Shell FilePath
jslibFileName basepath pkgid
  | pkgid `elem` specials = do
      dirs <- ls basepath
      case filter (and . zipWith (==) pkgid) dirs of
        (dir:_) -> do
          files <- ls (basepath </> dir)
          case filter (".jslib" `isSuffixOf`) files of
            (f:_) -> return $ dir </> f
            _     -> fail $ "Package " ++ pkgid ++ " has no jslib file!"
        _ -> do
          return stdname
  | otherwise = do
      mfile <- findLibFile [basepath]
      case mfile of
        Just f -> do
          -- Lib file was found directly in basepath
          return f
        _         -> do
          -- Lib file wasn't found directly in basepath, so let's try subdirs
          contents <- ls basepath
          let dirs = [basepath </> p | p <- contents, pkgid `isSuffixOf` p]
          dirs' <- filterM isDirectory dirs
          case dirs' of
            ds | not (null ds) -> maybe stdname id <$> findLibFile ds
               | otherwise     -> return stdname
  where
    findLibFile (d:ds) = do
      fs <- map (d </>) . filter (libfilesuffix `isSuffixOf`) <$> ls d
      fs' <- filterM isFile fs
      case fs' of
        (f:_) -> return (Just f)
        _     -> findLibFile ds
    findLibFile _ = do
      return Nothing

    -- Use this for non-special packages
    stdname = pkgid </> "libHS" ++ pkgid <.> "jslib"
    -- These package ids are special and come without version number
    specials = ["ghc-prim", "base", "integer-gmp"]
    libfilesuffix = pkgid <.> "jslib"

readMod :: FilePath -> String -> Bool -> Shell (Maybe Module)
readMod basepath modname boot = do
    x <- isFile path
    let path' = if x then path else syspath
    isF <- isFile path'
    if isF
       then do
         liftIO $ IO.withFile path' IO.ReadMode $ \h -> do
           m <- decode <$> (B.hGet h . fromInteger =<< IO.hFileSize h)
           m `seq` return (Just m)
       else do
         return Nothing
  where
    path = moduleFilePath "." modname boot
    syspath = moduleFilePath basepath modname boot

fromRight :: String -> IO (Either ExitReason b) -> IO b
fromRight from m = do
  ex <- m
  case ex of
    Right x -> return x
    Left e  -> fail $ "shell expression failed in " ++ from ++ ": " ++
                      exitString e
