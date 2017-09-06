#!/bin/bash

if [[ $1 == "" ]] ; then
    echo "Usage: ./packagelibs.sh version"
else
    mkdir -p libraries/ghc-7.10/array/include
    tar -cjf haste-libs-$1.tar.bz2 --exclude-backups '--exclude=*.hi' '--exclude=*.o' settings*.windows include utils/unlit/unlit.c libraries/rts.pkg libraries/haste-lib/{haste-lib.cabal,src/{Haste.hs,Haste},LICENSE} libraries/haste-prim/{src,LICENSE,Setup.hs,haste-prim.cabal} libraries/time/{lib,LICENSE,README,Setup.hs,time.cabal} libraries/ghc-7.10/{array/{array.cabal,Data,include,LICENSE,Setup.hs},base/{base.cabal,Control,Data,Debug,Foreign,Foreign.hs,GHC,Haste,include,LICENSE,Numeric*,Prelude.hs,Setup.hs,System,Text,Unsafe},ghc-prim/{ghc-prim.cabal,GHC,LICENSE,*.conf},integer-gmp/{integer-gmp.cabal,GHC,LICENSE,Setup.hs}}
fi
