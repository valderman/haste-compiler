module Config (Config (..), AppStart, defConfig, stdRtsLib, debugRtsLib,
               startASAP, startOnDocumentLoad) where
import CodeGen.Javascript (PrettyOpts, compact)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory
import System.FilePath (combine, replaceExtension)
import Paths_jsplug (getDataFileName)

type AppStart = String -> String

stdRtsLib :: FilePath
stdRtsLib = unsafePerformIO $ getDataFileName "rts.js"

debugRtsLib :: FilePath
debugRtsLib = unsafePerformIO $ getDataFileName "debug.js"

-- | Execute the program as soon as it's loaded into memory.
startASAP :: AppStart
startASAP mainSym =
  mainSym ++ "(0);"

-- | Execute the program when the document has finished loading.
startOnDocumentLoad :: AppStart
startOnDocumentLoad mainSym =
  "document.onload = function() {" ++ mainSym ++ "(0);};"

defaultLibPath :: FilePath
defaultLibPath = unsafePerformIO $ do
  appdir <- getAppUserDataDirectory "jsplug"
  return $ combine appdir "lib"

-- | Compiler configuration.
data Config = Config {
    -- | Runtime files to dump into the JS blob.
    rtsLibs :: [FilePath],
    -- | Path to directory where system libraries can be loaded or installed.
    libPath :: FilePath,
    -- | A function that takes the main symbol as its input and outputs the
    --   code that starts the program.
    appStart :: AppStart,
    -- | Options to the pretty printer.
    ppOpts :: PrettyOpts,
    -- | A function that takes the name of the a target as its input and
    --   outputs the name of the file its JS blob should be written to.
    outFile :: String -> String
  }

-- | Default compiler configuration.
defConfig :: Config
defConfig = Config {
    rtsLibs  = [stdRtsLib],
    libPath  = defaultLibPath,
    appStart = startASAP,
    ppOpts   = compact,
    outFile  = flip replaceExtension "js"
  }
