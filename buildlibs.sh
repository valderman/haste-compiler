#!/bin/bash
if [[ $1 == "" ]] ; then
    echo "Usage: $0 path_to_ghc_source"
    exit 1
fi

FLAGS="-O2 --unbooted"

libpath=$1/libraries

pushd .
cd $libpath/ghc-prim
hastec --libinstall $FLAGS -cpp -fglasgow-exts -package-name ghc-prim GHC.Types GHC.Classes GHC.IntWord64 GHC.Debug
popd

pushd .
cd $libpath/integer-simple
hastec --libinstall $FLAGS -cpp -fglasgow-exts -package-name integer-simple GHC.Integer GHC.Integer.Logarithms.Internals
popd

pushd .
cd $libpath/base
haste-inst configure --unbooted
haste-inst build --unbooted --install-jsmods
popd
