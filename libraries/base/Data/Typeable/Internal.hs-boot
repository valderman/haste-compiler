{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash #-}

module Data.Typeable.Internal (
    Typeable(typeOf),
    TypeRep,
    TyCon,
    mkTyCon,
    mkTyConApp
  ) where

import GHC.Base

data TypeRep
data TyCon

#include "MachDeps.h"

#if HASTE_HOST_WORD_SIZE_IN_BITS < 64 && __GLASGOW_HASKELL__ >= 706
mkTyCon :: a -> a -> String -> String -> String -> TyCon
#else
mkTyCon :: Word#   -> Word#   -> String -> String -> String -> TyCon
#endif

mkTyConApp   :: TyCon -> [TypeRep] -> TypeRep

class Typeable a where
  typeOf :: a -> TypeRep
