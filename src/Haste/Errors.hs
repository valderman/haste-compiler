-- | Functions for warning about and causing runtime errors.
module Haste.Errors (runtimeError, warn, WarnLevel(..)) where
import System.IO.Unsafe
import System.IO
import Data.JSTarget
import Haste.Monad
import Haste.Config

data WarnLevel = Normal | Verbose deriving Eq

-- | Produce a runtime error whenever this expression gets evaluated.
runtimeError :: String -> AST Exp
runtimeError s = callForeign "die" [lit s]

-- | Produce a warning message. This function is horrible and should be
--   replaced with some proper handling for warnings.
warn :: WarnLevel -> String -> JSGen Config ()
warn wlvl msg = do
  vrb <- verbose `fmap` getCfg
  if wlvl == Normal || vrb
    then return $! unsafePerformIO $ hPutStrLn stderr $ "WARNING: " ++ msg
    else return ()
