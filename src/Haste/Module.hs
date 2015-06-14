-- | Read and write JSMods.
module Haste.Module (writeModule, readModule) where
import Module (moduleNameSlashes, mkModuleName)
import qualified Data.ByteString.Lazy as B
import Control.Shell
import Control.Applicative
import Data.JSTarget
import Data.Binary
import qualified Data.ByteString.UTF8 as BS

-- | The file extension to use for modules.
jsmodExt :: Bool -> String
jsmodExt boot = if boot then "jsmod-boot" else "jsmod"

moduleFilePath :: FilePath -> String -> String -> Bool -> FilePath
moduleFilePath basepath pkgid modname boot =
  flip addExtension (jsmodExt boot) $
    basepath </> pkgid </> (moduleNameSlashes $ mkModuleName modname)

-- | Write a module to file, with the extension specified in `fileExt`.
--   Assuming that fileExt = "jsmod", a module Foo.Bar is written to
--   basepath/Foo/Bar.jsmod
--   If any directory in the path where the module is to be written doesn't
--   exist, it gets created.
writeModule :: FilePath -> Module -> Bool -> IO ()
writeModule basepath m@(Module pkgid modname _ _) boot =
  fromRight "writeModule" . shell $ do
    mkdir True (takeDirectory path)
    liftIO $ B.writeFile path (encode m)
  where
    path =
      moduleFilePath basepath (BS.toString pkgid) (BS.toString modname) boot

-- | Read a module from file. If the module is not found at the specified path,
--   libpath/path is tried instead. Returns Nothing is the module is not found
--   on either path.
readModule :: FilePath -> String -> String -> IO (Maybe Module)
readModule basepath pkgid modname = fromRight "readModule" . shell $ do
  mm <- readMod basepath pkgid modname False
  mmboot <- readMod basepath pkgid modname True
  case (mm, mmboot) of
    (Just m, Nothing)    -> return $ Just m
    (Just m, Just mboot) -> return . Just $ merge mboot m
    _                    -> return Nothing

readMod :: FilePath -> String -> String -> Bool -> Shell (Maybe Module)
readMod basepath pkgid modname boot = do
    x <- isFile path
    let path' = if x then path else syspath
    isF <- isFile path'
    if isF
       then Just . decode <$> liftIO (B.readFile path')
       else return Nothing
  where
    path = moduleFilePath "." pkgid modname boot
    syspath = moduleFilePath basepath pkgid modname boot

fromRight :: String -> IO (Either String b) -> IO b
fromRight from m = do
  ex <- m
  case ex of
    Right x -> return x
    Left e  -> fail $ "shell expression failed in " ++ from ++ ": " ++ e
