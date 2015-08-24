{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , ForeignFunctionInterface
           , EmptyDataDecls
           , MagicHash
           , UnliftedFFITypes
  #-}

-- ----------------------------------------------------------------------------
-- 
--  (c) The University of Glasgow 2006
--
-- Fingerprints for recompilation checking and ABI versioning, and
-- implementing fast comparison of Typeable.
--
-- ----------------------------------------------------------------------------

module GHC.Fingerprint (
        Fingerprint(..), fingerprint0, 
        fingerprintString,
        fingerprintFingerprints,
        getFileHash
   ) where

import GHC.IO
import GHC.Base
import GHC.Num
import GHC.List
import GHC.Real
import Foreign
import Foreign.C
import GHC.HastePrim
import GHC.IntWord64
import GHC.Word

import GHC.Fingerprint.Type

-- for SIZEOF_STRUCT_MD5CONTEXT:
#include "HsBaseConfig.h"

-- XXX instance Storable Fingerprint
-- defined in Foreign.Storable to avoid orphan instance

fingerprint0 :: Fingerprint
fingerprint0 = Fingerprint 0 0

fingerprintFingerprints :: [Fingerprint] -> Fingerprint
fingerprintFingerprints = md5 . concat . map showFingerprint

showFingerprint :: Fingerprint -> String
showFingerprint (Fingerprint (W64# a) (W64# b)) =
    unsafeCoerce# (fromJSStr# (md5# (jsShow# a))) ++
    unsafeCoerce# (fromJSStr# (md5# (jsShow# b)))

-- This is duplicated in compiler/utils/Fingerprint.hsc
fingerprintString :: String -> Fingerprint
fingerprintString = md5

foreign import ccall unsafe "md5" md5# :: JSString -> JSString
foreign import ccall unsafe "jsShowI" jsShow# :: Word64# -> JSString
#if __GLASGOW_HASKELL__ >= 706
foreign import ccall unsafe "parseInt" parseInt# :: JSString -> Word
#else
foreign import ccall unsafe "parseInt" _parseInt# :: JSString -> Int
parseInt# :: JSString -> Word
parseInt# = unsafeCoerce# _parseInt#
#endif

md5 :: String -> Fingerprint
md5 str =
    Fingerprint w64_1 w64_2
  where
    md5sum = unsafeCoerce# (fromJSStr# (md5# (toJSStr# (unsafeCoerce# str))))
    (s1, rest')   = splitAt 8 md5sum
    (s2, rest'')  = splitAt 8 rest'
    (s3, rest''') = splitAt 8 rest''
    (s4, _)       = splitAt 8 rest'''
    (w1,w2,w3,w4) = (parseHex s1, parseHex s2, parseHex s3, parseHex s3)
    w64_1         = mkWord64 w1 w2
    w64_2         = mkWord64 w3 w4

mkWord64 :: Word -> Word -> Word64
mkWord64 (W# w1) (W# w2) = W64# (mkWord64# w1 w2)

parseHex :: String -> Word
parseHex str = parseInt# (toJSStr ('0':'x':str))

getFileHash :: FilePath -> IO Fingerprint
getFileHash _ =
  return $ error "Fingerprinting a file does not make sense in a browser."
