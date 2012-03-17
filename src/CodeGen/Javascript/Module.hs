-- | Read and write JSMods.
module CodeGen.Javascript.Module (writeModule, readModule) where
import Module (moduleNameSlashes)
import qualified Data.ByteString as B
import CodeGen.Javascript.AST
import System.FilePath
import System.Directory
import Control.Applicative
import Data.Serialize

-- | The file extension to use for modules.
fileExt :: String
fileExt = "jsmod"

-- | Write a module to file, with the extension specified in `fileExt`.
--   Assuming that fileExt = "jsmod", a module Foo.Bar is written to
--   basepath/Foo/Bar.jsmod
--   If any directory in the path where the module is to be written doesn't
--   exist, it gets created.
writeModule :: FilePath -> JSMod -> IO ()
writeModule basepath m@(JSMod modName _ _) = do
  createDirectoryIfMissing True (takeDirectory path)
  B.writeFile (addExtension path fileExt) (encode m)
  where
    path = combine basepath $ moduleNameSlashes modName

-- | Read a module from file. If the module is not found at the specified path,
--   libpath/path is tried instead. Panics if the module is found on neither
--   path.
readModule :: FilePath -> FilePath -> IO JSMod
readModule libpath path = do
  x <- doesFileExist path
  let path' = if x then path else combine libpath path
  Right m <- decode <$> B.readFile path'
  return m
