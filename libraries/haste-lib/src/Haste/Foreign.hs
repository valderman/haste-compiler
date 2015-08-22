-- | High level JavaScript foreign interface.
module Haste.Foreign (
    -- * Conversion to/from JSAny
    ToAny (..), FromAny (..), JSAny,
    Opaque, toOpaque, fromOpaque,
    nullValue, toObject, has, get, index,

    -- * Importing and exporting JavaScript functions
    FFI, JSFunc,
    ffi, constant, export
  ) where
import Haste.Prim.Foreign
