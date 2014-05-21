#!/bin/bash

if [[ $1 == "" ]] ; then
    echo "Usage: ./packagelibs.sh version"
else
    tar -cjf haste-libs-$1.tar.bz2 --exclude-backups '--exclude=*.hi' '--exclude=*.o' include libraries/{array/{array.cabal,Data,include,LICENSE,Setup.hs},base/{base.cabal,Control,Data,Debug,Foreign,Foreign.hs,GHC,Haste,include,LICENSE,Numeric.hs,Prelude.hs,Setup.hs,System,Text,Unsafe},ghc-prim/{ghc-prim.cabal,GHC,LICENSE,packageconfig},haste-lib/{haste-lib.cabal,src/{Haste.hs,Haste},LICENSE},integer-gmp/{integer-gmp.cabal,GHC,LICENSE,Setup.hs},rts.pkg}
fi
