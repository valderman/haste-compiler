{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , ForeignFunctionInterface
           , GeneralizedNewtypeDeriving
           , NoImplicitPrelude
           , RecordWildCards
           , BangPatterns
  #-}

module GHC.Event.KQueue
    (
      new
    , available
    ) where

import qualified GHC.Event.Internal as E

import GHC.Base

new :: IO E.Backend
new = error "KQueue back end not implemented for this platform"

available :: Bool
available = False
{-# INLINE available #-}
