#!/bin/bash

runghc Setup.hs configure --user

if [ "$?" != "0" ] ; then
    echo "Can't configure Haste; you seem to be missing a dependency or two."
    echo "Attempting to get them from Hackage..."
    cabal update
    cabal install mtl cereal containers-0.4.2.1 bytestring filepath directory array ghc-paths

    runghc Setup.hs configure --user
    if [ "$?" != "0" ] ; then
        echo "Still can't configure Haste, and I have no idea why. :("
        exit 1
    fi
fi

runghc Setup.hs build && runghc Setup.hs install
if [ "$?" != "0" ] ; then
    echo "Unable to build Haste, and I have no idea why. :("
    exit 1
fi

# This step can't fail. I hope. At least not when downloading libs. Probably.
if [ "$1" != "" ] ; then
    echo "Local lib build requested, using GHC source from $1"
    ./buildlibs.sh $1
else
    echo "Downloading base libraries from ekblad.cc"
    pushd .
    mkdir -p ./libdl
    cd ./libdl
    TMPDIR=$(pwd)
    wget -N http://ekblad.cc/haste-libs.tar.bz2

    echo "Unpacking base libraries to ~/.haste"
    cd
    tar -xjf $TMPDIR/haste-libs.tar.bz2
    popd
fi

pushd .
cd ../fursuit
runghc Setup.hs configure --user && runghc Setup.hs build && runghc Setup.hs install
popd

# Install Haste again, to rebuild the Haste libs with the updated base libs.
runghc Setup.hs configure --user && runghc Setup.hs build && runghc Setup.hs install
