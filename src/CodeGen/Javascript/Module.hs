-- | Read and write JSMods.
module CodeGen.Javascript.Module (writeModule, readModule) where
import Module (moduleNameSlashes)
import Data.Binary
import qualified Data.ByteString.Lazy as B
import CodeGen.Javascript.AST
import CodeGen.Javascript.AST.Binary ()
import System.FilePath
import Control.Applicative

-- | The file extension to use for modules.
fileExt :: String
fileExt = "jsmod"

-- | Write a module to file, with the extension specified in `fileExt`.
--   Assuming that fileExt = "jsmod", a module Foo.Bar is written to
--   Foo/Bar.jsmod
writeModule :: JSMod -> IO ()
writeModule m@(JSMod modName _ _) =
  B.writeFile (addExtension path fileExt) (encode m)
  where
    path = moduleNameSlashes modName

-- | Read a module from file.
readModule :: FilePath -> IO JSMod
readModule path =
  decode <$> B.readFile path
