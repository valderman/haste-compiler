
{-# LANGUAGE NoImplicitPrelude, MagicHash #-}

module GHC.Char (chr) where

import GHC.Base
import GHC.Show
import GHC.HasteWordInt

-- | The 'Prelude.toEnum' method restricted to the type 'Data.Char.Char'.
chr :: Int -> Char
chr i@(I# i#)
 | isTrue# (i2w i# `leWord#` 0x10FFFF##) = C# (chr# i#)
 | otherwise
    = error ("Prelude.chr: bad argument: " ++ showSignedInt (I# 9#) i "")

