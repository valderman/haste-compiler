#!/bin/bash

# Install builtin RTS package, and create some dirs.
mkdir -p ~/.haste/haste-install/lib/ghc
cp -rf include ~/.haste/
haste-pkg update libraries/rts.pkg
if [ $? != 0 ] ; then
  echo "Failed while registering rts!"
  exit 1
fi

# Install custom ghc-prim.
pushd .
cd libraries/ghc-prim
haste-inst configure --unbooted
haste-inst build --unbooted --install-jsmods
if [ $? != 0 ] ; then
  echo "Failed while building ghc-prim!"
  exit 1
fi
haste-install-his ghc-prim-0.3.0.0 dist/build
if [ $? != 0 ] ; then
  echo "Failed while installing .hi files for ghc-prim!"
  exit 1
fi

haste-pkg update packageconfig
if [ $? != 0 ] ; then
  echo "Failed while registering ghc-prim!"
  exit 1
fi
popd

# Install custom integer-gmp.
pushd .
cd libraries/integer-gmp
haste-inst install --unbooted
haste-inst install --unbooted
if [ $? != 0 ] ; then
  echo "Failed while building integer-gmp!"
  exit 1
fi
popd

# Install haxxored base
pushd .
cd libraries/base
baseversion=$(cat base.cabal|egrep '^version'|awk '{print $2}')
haste-inst configure --unbooted
haste-inst build --unbooted --install-jsmods
if [ $? != 0 ] ; then
  echo "Failed while building base!"
  exit 1
fi
haste-install-his base-$baseversion dist/build
haste-copy-pkg base-$baseversion --package-db=dist/package.conf.inplace
if [ $? != 0 ] ; then
  echo "Failed while registering base!"
  exit 1
fi
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

haste-boot --force --no-libs --no-closure
