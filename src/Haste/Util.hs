{-# LANGUAGE CPP #-}
-- | Misc. utility functions.
module Haste.Util where
import DynFlags
import Outputable

showOutputable :: Outputable a => a -> String
#if __GLASGOW_HASKELL__ >= 706
showOutputable = showPpr tracingDynFlags
#else
showOutputable = showPpr
#endif

#if __GLASGOW_HASKELL__ >= 707
-- Taken from
-- http://www.haskell.org/ghc/docs/7.6.3/html/libraries/ghc/src/DynFlags.html#tracingDynFlags
--
-- Do not use tracingDynFlags!
-- tracingDynFlags is a hack, necessary because we need to be able to
-- show SDocs when tracing, but we don't always have DynFlags available.
-- Do not use it if you can help it. It will not reflect options set
-- by the commandline flags, and all fields may be either wrong or
-- undefined.
tracingDynFlags :: DynFlags
tracingDynFlags = defaultDynFlags tracingSettings
    where tracingSettings = panic "Settings not defined in tracingDynFlags"
#endif
