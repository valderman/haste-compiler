#!/bin/bash

if [[ $1 == "" ]] ; then
    echo "Usage: ./packagelibs.sh version"
else
    tar -cjf haste-libs-$1.tar.bz2 buildlibs.sh include libraries/{array,base,fursuit,ghc-prim,haste-lib,integer-gmp,rts.pkg}
fi
