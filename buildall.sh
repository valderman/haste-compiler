#!/bin/bash

# For some reason, HEAD requests to Google Code's servers always return 404,
# so we can't just wget -N compiler-latest.zip.
ALWAYS_REDOWNLOAD_CLOSURE=0

if [ -d ../fursuit ] ; then
    pushd .
    cd ../fursuit
    runghc Setup.hs configure --user && runghc Setup.hs build && runghc Setup.hs install
    popd
else
    echo "You don't seem to have the Fursuit sources in ../fursuit."
    echo "Fix that by running git clone git://github.com/valderman/fursuit.git in the parent directory."
    exit 1
fi

runghc Setup.hs configure --user

if [ "$?" != "0" ] ; then
    echo "Can't configure Haste; you seem to be missing a dependency or two."
    echo "Attempting to get them from Hackage..."
    cabal update
    cabal install mtl cereal containers-0.4.2.1 bytestring filepath directory array ghc-paths process

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

# Install Haste again, to rebuild the Haste libs with the updated base libs.
runghc Setup.hs configure --user && runghc Setup.hs build && runghc Setup.hs install

# Install fursuit again, for the libs
pushd .
cd ../fursuit
runghc Setup.hs configure --user && runghc Setup.hs build && runghc Setup.hs install
popd

# Get the Google Closure compiler
if [[ ! -e ~/.haste/lib/compiler.jar || "$ALWAYS_REDOWNLOAD_CLOSURE" == "1" ]] ; then
    echo "Closure compiler not found, or override specified. Downloading."
    pushd .
    cd ~/.haste/lib
    wget http://closure-compiler.googlecode.com/files/compiler-latest.zip
    unzip -o compiler-latest.zip
    if [ "$?" != "0" ] ; then
        echo "Unable to fetch the Google Closure compiler."
        echo "You won't be able to compile with --opt-google-closure."
        echo "Use --opt-google-closure=/path/to/closure/compiler.jar instead."
    else
        rm COPYING README compiler-latest.zip
    fi
    popd
else
    echo "Closure compiler already installed, not downloading."
fi
