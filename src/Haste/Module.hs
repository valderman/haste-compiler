-- | Read and write JSMods.
module Haste.Module (writeModule, readModule, readModuleFingerprint) where
import Module (moduleNameSlashes, mkModuleName)
import qualified Data.ByteString.Lazy as B
import Control.Shell
import System.IO
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
readModuleFingerprint basepath pkgid modname = fromRight . shell $ do
    x <- isFile path
    let path' = if x then path else syspath 
    liftIO $ withFile path' ReadMode $ \h -> do
      fp <- decode <$> B.hGetContents h
      return $! fp
  where
    path = moduleFilePath "" "" modname
    syspath = moduleFilePath basepath pkgid modname

-- | Write a module to file, with the extension specified in `fileExt`.
--   Assuming that fileExt = "jsmod", a module Foo.Bar is written to
--   basepath/Foo/Bar.jsmod
--   If any directory in the path where the module is to be written doesn't
--   exist, it gets created.
writeModule :: FilePath -> Module -> IO ()
writeModule basepath m@(Module _ pkgid modname _ _) = fromRight . shell $ do
    mkdir True (takeDirectory path)
    liftIO $ B.writeFile path (encode m)
  where
    path = moduleFilePath basepath pkgid modname

-- | Read a module from file. If the module is not found at the specified path,
--   libpath/path is tried instead. Panics if the module is found on neither
--   path.
readModule :: FilePath -> String -> String -> IO Module
readModule basepath pkgid modname = fromRight . shell $ do
    x <- isFile path
    let path' = if x then path else syspath 
    decode <$> liftIO (B.readFile path')
  where
    path = moduleFilePath "." pkgid modname
    syspath = moduleFilePath basepath pkgid modname

fromRight :: IO (Either a b) -> IO b
fromRight m = do Right x <- m ; return x
