#!/bin/bash
if [[ $1 == "" ]] ; then
    echo "Usage: $0 path_to_ghc_source"
    exit 1
fi

FLAGS=-O2

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
hastec --libinstall $FLAGS -hide-package base -package-name base -I./include -i./dist-install/build -XMagicHash -XExistentialQuantification -XRank2Types -XScopedTypeVariables -XUnboxedTuples -XForeignFunctionInterface -XUnliftedFFITypes -XDeriveDataTypeable -XGeneralizedNewtypeDeriving -XFlexibleInstances -XStandaloneDeriving -XPatternGuards -XEmptyDataDecls -XNoImplicitPrelude -XCPP Prelude
popd
