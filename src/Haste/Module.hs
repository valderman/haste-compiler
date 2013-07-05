-- | Read and write JSMods.
module Haste.Module (writeModule, readModule, readModuleFingerprint) where
import Module (moduleNameSlashes, mkModuleName)
import qualified Data.ByteString.Lazy as B
import System.FilePath
import System.Directory
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

readModuleFingerprint :: FilePath -> String -> String -> IO Fingerprint
readModuleFingerprint basepath pkgid modname = do
    x <- doesFileExist path
    let path' = if x then path else syspath 
    decode <$> B.readFile path'
  where
    path = moduleFilePath "" "" modname
    syspath = moduleFilePath basepath pkgid modname

-- | Write a module to file, with the extension specified in `fileExt`.
--   Assuming that fileExt = "jsmod", a module Foo.Bar is written to
--   basepath/Foo/Bar.jsmod
--   If any directory in the path where the module is to be written doesn't
--   exist, it gets created.
writeModule :: FilePath -> Module -> IO ()
writeModule basepath m@(Module _ pkgid modname _ _) = do
    createDirectoryIfMissing True (takeDirectory path)
    B.writeFile path (encode m)
  where
    path = moduleFilePath basepath pkgid modname

-- | Read a module from file. If the module is not found at the specified path,
--   libpath/path is tried instead. Panics if the module is found on neither
--   path.
readModule :: FilePath -> String -> String -> IO Module
readModule basepath pkgid modname = do
    x <- doesFileExist path
    let path' = if x then path else syspath 
    decode <$> B.readFile path'
  where
    path = moduleFilePath "." pkgid modname
    syspath = moduleFilePath basepath pkgid modname
