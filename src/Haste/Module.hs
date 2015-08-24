{-# LANGUAGE CPP #-}
-- | Read and write JSMods.
module Haste.Module (writeModule, readModule) where
import Module (moduleNameSlashes, mkModuleName)
import qualified Data.ByteString.Lazy as B
import Control.Shell
import Control.Applicative
import Control.Monad (when, filterM)
import Data.JSTarget
import Data.Binary
import Data.List (isSuffixOf)
import qualified Data.ByteString.UTF8 as BS
import qualified Haste.JSLib as JSLib
import qualified System.IO as IO

-- | The file extension to use for modules.
jsmodExt :: Bool -> String
jsmodExt boot = if boot then "jsmod-boot" else "jsmod"

moduleFilePath :: FilePath -> String -> String -> Bool -> FilePath
moduleFilePath basepath pkgid modname boot =
  flip addExtension (jsmodExt boot) $
    basepath </> pkgid </> (moduleNameSlashes $ mkModuleName modname)

-- | Write a module to file, with the extension specified in `fileExt`.
--   Assuming that fileExt = "jsmod", a module Foo.Bar is written to
--   @basepath/Foo/Bar.jsmod@.
--
--   If any directory in the path where the module is to be written doesn't
--   exist, it gets created.
--
--   Boot modules and "normal" modules get merged at this stage.
writeModule :: FilePath -> Module -> Bool -> IO ()
writeModule basepath m@(Module pkgid modname _ _) boot =
  fromRight "writeModule" . shell $ do
    mkdir True (takeDirectory path)
    mcompanion <- readMod basepath pkgstr modstr (not boot)
    m' <- case mcompanion of
            Just companion -> do
              bootfileExists <- isFile bootpath
              when bootfileExists $ rm bootpath
              return $ merge' m companion
            _              -> do
              return m
    liftIO . B.writeFile path $ encode m'
  where
    pkgstr = BS.toString pkgid
    modstr = BS.toString modname
    path = moduleFilePath basepath pkgstr modstr boot
    bootpath = moduleFilePath basepath pkgstr modstr True
    merge' = if boot then merge else flip merge

-- | Read a module from file. If the module is not found at the specified path,
--   libpath/path is tried instead. Returns Nothing is the module is not found
--   on either path.
readModule :: FilePath -> String -> String -> IO (Maybe Module)
readModule basepath pkgid modname = fromRight "readModule" . shell $ do
  libfile <- (basepath </>) `fmap` jslibFileName basepath pkgid
  mmlib <- liftIO $ JSLib.readModule libfile modname
  mm <- readMod basepath pkgid modname False
  mmboot <- readMod basepath pkgid modname True
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
#if __GLASGOW_HASKELL__ < 709
      return stdname
#else
      dirs <- filter (pkgid `isSuffixOf`) . map (basepath </>) <$> ls basepath
      dirs' <- filterM isDirectory dirs
      case dirs' of
        ds | not (null ds) -> findLibFile ds
           | otherwise     -> return stdname
#endif
  where
    findLibFile (d:ds) = do
      fs <- map (d </>) . filter (libfilesuffix `isSuffixOf`) <$> ls d
      fs' <- filterM isFile fs
      case fs' of
        (f:_) -> return f
        _     -> findLibFile ds
    findLibFile _ = do
      return stdname

    -- Use this for non-special packages
    stdname = pkgid </> "libHS" ++ pkgid <.> "jslib"
    -- These package ids are special and come without version number
    specials = ["ghc-prim", "base", "integer-gmp"]
    libfilesuffix = pkgid <.> "jslib"

readMod :: FilePath -> String -> String -> Bool -> Shell (Maybe Module)
readMod basepath pkgid modname boot = do
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
    path = moduleFilePath "." pkgid modname boot
    syspath = moduleFilePath basepath pkgid modname boot

fromRight :: String -> IO (Either String b) -> IO b
fromRight from m = do
  ex <- m
  case ex of
    Right x -> return x
    Left e  -> fail $ "shell expression failed in " ++ from ++ ": " ++ e
