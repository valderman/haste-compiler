#!/bin/bash
if [[ $1 == "" ]] ; then
    echo "Usage: $0 path_to_ghc_source"
    exit 1
fi

libpath=$1/libraries

# Install builtin RTS package, and create some dirs.
mkdir -p ~/.haste/haste-install/lib/ghc
haste-copy-pkg rts-1.0 rts

# Install custom ghc-prim.
pushd .
cd libraries/ghc-prim
haste-inst configure --unbooted
haste-inst build --unbooted --install-jsmods
haste-install-his ghc-prim-0.3.0.0 dist/build
haste-pkg update packageconfig
popd

# Install custom integer-gmp.
pushd .
cd libraries/integer-gmp
haste-inst install --unbooted
popd

# Install base from GHC source. GHC needs to be built beforehand.
pushd .
cd $libpath/base
baseversion=$(cat base.cabal|egrep '^version'|awk '{print $2}')
haste-inst configure --unbooted
haste-inst build --unbooted --install-jsmods
haste-install-his base-$baseversion dist/build
haste-copy-pkg base-$baseversion --package-db=dist/package.conf.inplace
popd

# Install fursuit, since haste-lib depends on it.
pushd .
tempdir=$(mktemp -d)
cd $tempdir
git clone https://github.com/valderman/fursuit.git
cd fursuit
haste-inst install --unbooted
popd
rm -rf $tempdir

# Install haste-lib.
pushd .
cd libraries/haste-lib
haste-inst install --unbooted
popd

# Fetch google Closure compiler and mark Haste as booted.
haste-boot --force --no-haste --no-base
