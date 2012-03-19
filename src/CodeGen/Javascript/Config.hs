module CodeGen.Javascript.Config (
  Config (..), AppStart, defConfig, stdRtsLib, stdJSLib, startASAP,
  startOnDocumentLoad, appName, sysLibPath) where
import CodeGen.Javascript.PrettyM (PrettyOpts, compact)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory
import System.FilePath (combine, replaceExtension)
import Control.Applicative
import Paths_haste_compiler (getDataFileName)

type AppStart = String -> String

stdRtsLib :: FilePath
stdRtsLib = unsafePerformIO $ getDataFileName "rts.js"

stdJSLib :: FilePath
stdJSLib = unsafePerformIO $ getDataFileName "stdlib.js"

-- | Name of the application; decides which directories to keep app specific
--   data in.
appName :: String
appName = "haste"

append :: FilePath -> FilePath -> FilePath
append = flip combine

-- | Execute the program as soon as it's loaded into memory.
startASAP :: AppStart
startASAP mainSym =
  "E(" ++ mainSym ++ ")(0);"

-- | Execute the program when the document has finished loading.
startOnDocumentLoad :: AppStart
startOnDocumentLoad mainSym =
  "document.onload = function() {" ++ startASAP mainSym ++ "};"

sysLibPath :: FilePath
sysLibPath = unsafePerformIO $ do
  append "lib" <$> getAppUserDataDirectory appName

-- | Compiler configuration.
data Config = Config {
    -- | Runtime files to dump into the JS blob.
    rtsLibs :: [FilePath],
    -- | Path to directory where system libraries are located.
    libPath :: FilePath,
    -- | Write all modules to this path.
    targetLibPath :: FilePath,
    -- | A function that takes the main symbol as its input and outputs the
    --   code that starts the program.
    appStart :: AppStart,
    -- | Options to the pretty printer.
    ppOpts :: PrettyOpts,
    -- | A function that takes the name of the a target as its input and
    --   outputs the name of the file its JS blob should be written to.
    outFile :: String -> String,
    -- | Link the program?
    performLink :: Bool
  }

-- | Default compiler configuration.
defConfig :: Config
defConfig = Config {
    rtsLibs       = [stdRtsLib,stdJSLib],
    libPath       = sysLibPath,
    targetLibPath = ".",
    appStart      = startASAP,
    ppOpts        = compact,
    outFile       = flip replaceExtension "js",
    performLink   = True
  }
