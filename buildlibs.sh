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
hastec --libinstall $FLAGS -hide-package base -package-name base -I./include -i./dist-install/build -XMagicHash -XExistentialQuantification -XRank2Types -XScopedTypeVariables -XUnboxedTuples -XForeignFunctionInterface -XUnliftedFFITypes -XDeriveDataTypeable -XGeneralizedNewtypeDeriving -XFlexibleInstances -XStandaloneDeriving -XPatternGuards -XEmptyDataDecls -XNoImplicitPrelude -XCPP Data.Word Prelude Control.Applicative Data.Bits Data.Char Data.Either Data.Functor Data.HashTable Data.IORef Data.Maybe Data.Monoid Data.Ord Data.STRef.Lazy Data.STRef.Strict Data.STRef Control.Monad.Instances Data.Fixed System.CPUTime
popd

pushd .
cd $libpath/time
hastec --libinstall $FLAGS -package-name time -cpp -fglasgow-exts -I./include Data.Time
popd

pushd .
cd $libpath/array
hastec --libinstall $FLAGS -package-name array -cpp -fglasgow-exts -I./include Data.Array Data.Array.MArray Data.Array.IArray Data.Array.IO Data.Array.ST Data.Array.Storable Data.Array.Unboxed Data.Array.Unsafe
popd

pushd .
cd $libpath/containers
hastec --libinstall $FLAGS -cpp -fglasgow-exts -package-name containers Data.Graph Data.IntMap Data.IntSet Data.Map Data.Sequence Data.Set Data.Tree
popd
