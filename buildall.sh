#!/bin/bash
runghc Setup.hs configure --user && runghc Setup.hs build && runghc Setup.hs install

if [ "$1" != "" ] ; then
  ./buildlibs.sh $1
fi

pushd .
cd ../fursuit
runghc Setup.hs configure --user && runghc Setup.hs build && runghc Setup.hs install
popd
