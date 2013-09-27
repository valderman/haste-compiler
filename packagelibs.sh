#!/bin/bash

if [[ $1 == "" ]] ; then
    echo "Usage: ./packagelibs.sh version"
else
    tar -cjf haste-libs-$1.tar.bz2 include libraries/{array/{array.cabal,Data,include,LICENSE,Setup.hs},base/{base.cabal,Control,Data,Debug,Foreign,Foreign.hs,GHC,Haste,include,LICENSE,Numeric.hs,Prelude.hs,Setup.hs,System,Text,Unsafe},fursuit/{fursuit.cabal,Setup.hs,src},ghc-prim/{ghc-prim.cabal,GHC,LICENSE,packageconfig},haste-lib/{haste-lib.cabal,src,LICENSE},integer-gmp/{integer-gmp.cabal,GHC,LICENSE,Setup.hs},rts.pkg}
fi
