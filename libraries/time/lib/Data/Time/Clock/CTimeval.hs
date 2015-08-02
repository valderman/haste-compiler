{-# LANGUAGE OverloadedStrings #-}
module Data.Time.Clock.CTimeval where
import Haste.Foreign
import Unsafe.Coerce

#if __GLASGOW_HASKELL__ >= 709
import Foreign
#else
import Foreign.Safe
#endif
import Foreign.C

data CTimeval = MkCTimeval CLong CLong

instance FromAny CTimeval where
  fromAny x = do
    s <- index x 0 :: IO Int
    us <- index x 1 :: IO Int
    -- CLong and Int have the same representation
    return $! s `seq` us `seq` MkCTimeval (unsafeCoerce s) (unsafeCoerce us)

instance Storable CTimeval where
	sizeOf _ = (sizeOf (undefined :: CLong)) * 2
	alignment _ = alignment (undefined :: CLong)
	peek p = do
		s   <- peekElemOff (castPtr p) 0
		mus <- peekElemOff (castPtr p) 1
		return (MkCTimeval s mus)
	poke p (MkCTimeval s mus) = do
		pokeElemOff (castPtr p) 0 s
		pokeElemOff (castPtr p) 1 mus

-- | Get the current POSIX time from the system clock.
getCTimeval :: IO CTimeval
getCTimeval =
  ffi "(function(){var ms = new Date().getTime();\
                   return [(ms/1000)|0, ((ms % 1000)*1000)|0];})"
