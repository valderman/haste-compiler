{-# LANGUAGE OverloadedStrings #-}
-- | (Very incomplete) Haste bindings to the @Performance@ DOM interface.
module Haste.Performance (HRTimeStamp, now, navigationStart) where
import Haste.Prim.Foreign

type HRTimeStamp = Double

-- | Returns the number of milliseconds since 'navigationStart', with
--   (theoretically) microsecond precision.
now :: IO HRTimeStamp
now = ffi "(function(){return performance.now();})"

-- | Returns the number of milliseconds elapsed since UNIX epoch at the moment
--   when this document started loading.
navigationStart :: IO Double
navigationStart = ffi "(function(){return performance.timing.navigationStart;})"
