-- | Read and write JSMods.
module Haste.Module (writeModule, readModule) where
import Module (moduleNameSlashes, mkModuleName)
import qualified Data.ByteString.Lazy as B
import Control.Shell
import Control.Applicative
import Data.JSTarget
import Data.Binary

-- | The file extension to use for modules.
jsmodExt :: String
jsmodExt = "jsmod"

moduleFilePath :: FilePath -> String -> String -> FilePath
moduleFilePath basepath pkgid modname =
  flip addExtension jsmodExt $
    basepath </> pkgid </> (moduleNameSlashes $ mkModuleName modname)

-- | Write a module to file, with the extension specified in `fileExt`.
--   Assuming that fileExt = "jsmod", a module Foo.Bar is written to
--   basepath/Foo/Bar.jsmod
--   If any directory in the path where the module is to be written doesn't
--   exist, it gets created.
writeModule :: FilePath -> Module -> IO ()
writeModule basepath m@(Module pkgid modname _ _) =
  fromRight "writeModule" . shell $ do
    mkdir True (takeDirectory path)
    liftIO $ B.writeFile path (encode m)
  where
    path = moduleFilePath basepath pkgid modname

-- | Read a module from file. If the module is not found at the specified path,
--   libpath/path is tried instead. Panics if the module is found on neither
--   path.
readModule :: FilePath -> String -> String -> IO (Maybe Module)
readModule basepath pkgid modname = fromRight "readModule" . shell $ do
    x <- isFile path
    let path' = if x then path else syspath
    isF <- isFile path'
    if isF
       then Just . decode <$> liftIO (B.readFile path')
       else return Nothing
  where
    path = moduleFilePath "." pkgid modname
    syspath = moduleFilePath basepath pkgid modname

fromRight :: String -> IO (Either String b) -> IO b
fromRight from m = do
  ex <- m
  case ex of
    Right x -> return x
    Left e  -> fail $ "shell expression failed in " ++ from ++ ": " ++ e
