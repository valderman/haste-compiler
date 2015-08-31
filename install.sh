#!/bin/sh
INSTDIR=/usr/local/lib/haste-compiler
BINDIR=/usr/local/bin
MANDIR=/usr/local/share/man/man1

if [ "$(whoami)" != "root" ] ; then
    echo "Haste installer must be run as root!"
    exit 1
fi

if [ -f haste-cabal/haste-cabal.bin ] ; then
    mkdir -p $INSTDIR
    mkdir -p $MANDIR
    cp -r bin docs haste-cabal man uninstall.sh x86_64-linux-haste* $INSTDIR/
    ln -s $INSTDIR/bin/hastec $BINDIR/hastec
    ln -s $INSTDIR/bin/haste-cat $BINDIR/haste-cat
    ln -s $INSTDIR/bin/haste-cabal $BINDIR/haste-cabal
    ln -s $INSTDIR/bin/haste-pkg $BINDIR/haste-pkg
    ln -s $INSTDIR/man/hastec.1 $MANDIR/hastec.1
    ln -s $INSTDIR/man/haste-cat.1 $MANDIR/haste-cat.1
    echo "All done, enjoy using the Haste compiler!"
else
    echo "Please run the Haste installer from the root of the unpacked"
    echo "binary distribution."
    exit 1
fi
