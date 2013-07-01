-- | Read and write JSMods.
module Haste.Module (writeModule, readModule) where
import Module (moduleNameSlashes, mkModuleName)
import qualified Data.ByteString.Lazy as B
import System.FilePath
import System.Directory
import Control.Applicative
import Data.JSTarget
import Data.Binary

-- | The file extension to use for modules.
fileExt :: String
fileExt = "jsmod"

-- | Write a module to file, with the extension specified in `fileExt`.
--   Assuming that fileExt = "jsmod", a module Foo.Bar is written to
--   basepath/Foo/Bar.jsmod
--   If any directory in the path where the module is to be written doesn't
--   exist, it gets created.
writeModule :: FilePath -> Module -> IO ()
writeModule basepath m@(Module modname _ _) = do
  createDirectoryIfMissing True (takeDirectory path)
  B.writeFile (addExtension path fileExt) (encode m)
  where
    path = combine basepath $ moduleNameSlashes $ mkModuleName $ modname

-- | Read a module from file. If the module is not found at the specified path,
--   libpath/path is tried instead. Panics if the module is found on neither
--   path.
readModule :: FilePath -> FilePath -> IO Module
readModule libpath path = do
  x <- doesFileExist path
  let path' = if x then path else combine libpath path
  decode <$> B.readFile path'
