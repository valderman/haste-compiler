{-# LANGUAGE CPP #-}
-- | Misc. utility functions.
module Haste.Util where
import DynFlags
import Outputable

#if __GLASGOW_HASKELL__ >= 706
showOutputable :: Outputable a => a -> String
showOutputable = showPpr tracingDynFlags
#else
showOutputable :: Outputable a => a -> String
showOutputable = showPpr
#endif
