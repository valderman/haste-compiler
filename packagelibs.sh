#!/bin/bash

if [[ $1 == "" ]] ; then
    echo Usage: $0 path_to_ghc_source
    exit 1
fi

restore_backup=no

# Set some directories
build_dir=$(mktemp -d)
working_dir=$(pwd)

# Back up the old ~/.haste directory if there is one, then create a new one
# in /tmp and link it to ~/.haste.
if [ -d ~/.haste ] ; then
    backup_dir=$(mktemp -d -p ~)
    mv ~/.haste $backup_dir/
    restore_backup=yes
fi
ln -s $build_dir ~/.haste

# Build libraries, etc.
./buildlibs.sh $1 --only-base

# Pack it all up
cd ~
tar -cjhf $working_dir/haste-libs.tar.bz2 .haste/
cd $working_dir

# Remove build dir and restore old .haste dir.
rm -rf $build_dir
rm ~/.haste

if [[ $restore_backup == "yes" ]] ; then
    mv $backup_dir/.haste ~/
    rmdir $backup_dir
fi
