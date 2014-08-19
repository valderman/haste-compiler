{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
module GHC.HasteWordInt where
import GHC.Prim

{-# NOINLINE w2i #-}
w2i :: Word# -> Int#
w2i w = word2Int# w

{-# NOINLINE i2w #-}
i2w :: Int# -> Word#
i2w i = int2Word# i
