#!/bin/bash
runghc Setup.hs configure --user && runghc Setup.hs build && runghc Setup.hs install

if [ "$1" != "" ] ; then
  ./buildlibs.sh $1
else
  TMPDIR=$(mktemp -d)
  wget -O$TMPDIR/haste-libs.tar.bz2 http://ekblad.cc/haste-libs.tar.bz2
  pushd .
  cd
  tar -xjf $TMPDIR/haste-libs.tar.bz2
  popd
  rm -r $TMPDIR
fi

pushd .
cd ../fursuit
runghc Setup.hs configure --user && runghc Setup.hs build && runghc Setup.hs install
popd

# Install Haste again, to rebuild the Haste libs with the updated base libs.
runghc Setup.hs configure --user && runghc Setup.hs build && runghc Setup.hs install
