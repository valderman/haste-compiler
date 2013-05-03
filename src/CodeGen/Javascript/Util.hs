-- | Misc. utility functions.
module CodeGen.Javascript.Util where
import DynFlags
import Outputable

showOutputable :: Outputable a => a -> String
showOutputable = showPpr tracingDynFlags

