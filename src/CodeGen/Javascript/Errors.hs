-- | Functions for warning about and causing runtime errors.
module CodeGen.Javascript.Errors (runtimeError, warn, WarnLevel(..)) where
import System.IO.Unsafe
import System.IO
import CodeGen.Javascript.AST
import CodeGen.Javascript.Monad
import CodeGen.Javascript.Config

data WarnLevel = Normal | Verbose deriving Eq

-- | Produce a runtime error whenever this expression gets evaluated.
runtimeError :: String -> JSExp
runtimeError s = NativeCall "die" [lit s]

-- | Produce a warning message. This function is horrible and should be
--   replaced with some proper handling for warnings.
warn :: WarnLevel -> String -> JSGen Config ()
warn wlvl msg = do
  vrb <- verbose `fmap` getCfg
  if wlvl == Normal || vrb
    then return $! unsafePerformIO $ hPutStrLn stderr $ "WARNING: " ++ msg
    else return ()
