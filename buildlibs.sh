#!/bin/bash

# Install builtin RTS package, and create some dirs.
mkdir -p ~/.haste/haste-install/lib/ghc
cp -rf include ~/.haste/
haste-pkg update libraries/rts.pkg

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
haste-inst install --unbooted
popd

# Install haxxored base
pushd .
cd libraries/base
baseversion=$(cat base.cabal|egrep '^version'|awk '{print $2}')
haste-inst configure --unbooted
haste-inst build --unbooted --install-jsmods
haste-install-his base-$baseversion dist/build
haste-copy-pkg base-$baseversion --package-db=dist/package.conf.inplace
popd

# Install haxored array
pushd .
cd libraries/array
haste-inst install --unbooted
popd

# If we were only asked to install base, then we're done now.
if [[ $2 == "--only-base" ]] ; then
    exit 0
fi

# Install fursuit, since haste-lib depends on it.
pushd .
cd libraries/fursuit
haste-inst install --unbooted
popd

# Install haste-lib.
pushd .
cd libraries/haste-lib
haste-inst install --unbooted
popd

haste-boot --force --no-haste --no-base
