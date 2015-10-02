#!/bin/sh
HASTEDIR=$(dirname "$(hastec --print-libdir 2> /dev/null)")
BINDIR=/usr/local/bin
MANDIR=/usr/local/share/man/man1
if [ "$(whoami)" != "root" ] ; then
    echo "Haste uninstaller must be run as root!"
    exit 1
fi

if [ "$HASTEDIR" = "." ] ; then
    echo "Haste does not seem to be installed."
    exit 1
fi

if [ -f $HASTEDIR/haste-cabal/haste-cabal.bin ] ||
   [ -f $HASTEDIR/bin/hastec ] ; then
    rm $BINDIR/hastec
    rm $BINDIR/haste-cat
    rm $BINDIR/haste-cabal
    rm $BINDIR/haste-pkg
    rm $MANDIR/hastec.1
    rm $MANDIR/haste-cat.1
    rm -r $HASTEDIR
    echo "Haste is uninstalled!"
else
    echo "Haste does not seem to be installed."
    exit 1
fi
