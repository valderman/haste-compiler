-- | Misc. utility functions.
module Haste.Util where
import DynFlags
import Outputable

showOutputable :: Outputable a => a -> String
showOutputable = showPpr tracingDynFlags

