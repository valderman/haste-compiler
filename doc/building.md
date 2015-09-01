Building and booting the Haste compiler
=======================================

This document briefly describes how to build Haste from scratch.
This is fairly easy on Linux-based systems, and a bit less easy on Windows.


Build dependencies
------------------

Build dependencies all non-Haskell dependencies necessary to build as well as
bootstrap Haste.

### For all platforms

  * libbz2
  * zlib
  * git
  * GHC >= 7.8 && < 7.11
  * libgmp
  * libffi

### Additional Windows dependencies

  * Cygwin, including GCC

### Additional OSX dependencies

  * Apple's XCode developer kit


Building `haste-cabal`
----------------------

As Haste support is not yet merged into vanilla Cabal, for now we have to use
our own `haste-cabal` wrapper. An appropriate pre-built version of this binary
is downloaded as part of Haste's bootstrap process, so building it yourself
is not strictly necessary to build Haste from scratch.
However, if you experience problems with the pre-built binaries, building from
source is always a good idea.

Start by checking out the source and switching to the appropriate branch:

    $ git clone https://github.com/valderman/cabal && cd cabal
    $ git checkout haste-cabal

Then we build and install the Cabal library:

    $ cd Cabal && cabal install

Finally, install the modified cabal binary itself:

    $ cd ../cabal-install && cabal install

On Linux, `haste-cabal` has a runtime dependency on the versions of `libgmp`
and `libffi` it was linked against. If you intend to run your binary on
another machine than the one you built it on, you may want to bundle those
libraries with your binary and use a wrapper script to execute it with the
appropriate library environment.


Building Haste
--------------

If you are building a released version, simply use cabal and you're done:

    $ cabal update && cabal install haste-compiler

If you are building a development branch, or if you want to build a portable
binary package, you should start by checking out the source from GitHub:

    $ git clone https://github.com/valderman/haste-compiler && cd haste-compiler

If you simply want to install the current development version, just install
it with cabal as usual:

    $ cabal install

If you want to build a portable binary package, the process is slightly
different:

    $ cabal configure -fportable -fstatic
    $ cabal build

All binaries will be build and copied into the `haste-compiler` directory
in the repository root.

The `portable` flag tells Haste to look for libraries and settings in the
directory where the `hastec` binary resides, while the `static` flag enables
building the Haste binaries statically. On OSX, the latter flag doesn't do
anything as OSX is rather hostile towards static linking.

Be aware that a portable installation is **statically linked** (except on OSX),
and thus includes `libgmp`. This comes with a small legal gotcha:

  * you will need the static `libgmp` libraries (`.a` files) to build, and
  * if you are distributing portable Haste binaries with proprietary
    modifications, *you are violating the LGPL license of libgmp* unless you
    also provide your application in source or object format.
    If this is a problem for you, consider contributing your changes back to
    mainline Haste under the BSD3 license.


Booting Haste
-------------

### Normal install

Booting Haste is now a matter of running the appropriate `haste-boot` binary.
If you installed the Haste binaries onto your system (as opposed to building
the portable binaries), you just add Cabal's local binary directory to your
search path, and then run `haste-boot --local` from the root of your source
repository, indicating that you want to use the libraries already present
in that directory instead of downloading some other version.

If you built and installed your own `haste-cabal` binary, you will want to
tell `haste-boot` to avoid downloading a pre-built `haste-cabal` binary
using the `--no-haste-cabal` flag.

See `haste-boot --help` for more bootstrapping options.

### Portable

If you built the portable binaries, you instead need to run the `haste-boot`
binary that got copied into `haste-compiler` (which is highly unlikely to be
on your search path):

    $ haste-boot/bin/haste-boot --local

Haste will be bootstrapped into said directory rather than into `~/.haste`.

If you built your own `haste-cabal` binary, you will want to copy that binary
into this directory as well, and pass the `--no-haste-cabal` flag to ensure
that `haste-boot` does not overwrite your binary with a pre-built one.


Building a complete portable package
------------------------------------

The process of checking out a clean copy of Haste, building, booting and
packing up the result into a redistributable binary package is automated using
the `build-release.hs` script.
In order to build a portable Debian package, for instance, simply run
`runghc build-release deb` and a Debian package will appear in the
`ghc-7.(8|10)` directory in the root of Haste's source tree, depending on
which version of GHC you used.

The build-release script can build multiple file formats in one go; simple
pass all formats you wish to build as command line arguments.
To see which formats are currently supported by build-release, run it without
arguments.
