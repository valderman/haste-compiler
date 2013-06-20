{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , ForeignFunctionInterface
           , GeneralizedNewtypeDeriving
           , NoImplicitPrelude
           , BangPatterns
  #-}

-----------------------------------------------------------------------------
-- |
-- A binding to the epoll I/O event notification facility
--
-- epoll is a variant of poll that can be used either as an edge-triggered or
-- a level-triggered interface and scales well to large numbers of watched file
-- descriptors.
--
-- epoll decouples monitor an fd from the process of registering it.
--
-----------------------------------------------------------------------------

module GHC.Event.EPoll
    (
      new
    , available
    ) where

import qualified GHC.Event.Internal as E

import GHC.Base

new :: IO E.Backend
new = error "EPoll back end not implemented for this platform"

available :: Bool
available = False
{-# INLINE available #-}
