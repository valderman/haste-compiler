Haste
=====

A compiler to generate Javascript code from Haskell.


Building
--------

Clone Haste and Fursuit into the same directory, then cd to
$THAT_DIRECTORY/haste-compiler and run ./buildall.sh. This should, hopefully,
set everything up for you. If that didn't make sense to you, just run the
following commands in your terminal of choice:

    $ mkdir haste && cd haste
    $ git clone git://github.com/valderman/haste-compiler.git
    $ git clone git://github.com/valderman/fursuit.git
    $ cd haste-compiler && ./buildall.sh

This will check out the compiler and its dependencies, download pre-built
standard libraries from ekblad.cc, and install everything locally, for your
user only. The binaries will be in ~/.cabal/bin, the base libraries in
~/.haste/lib and the supporting JS run time system in
~/.cabal/share/haste-compiler-$VERSION.

I'd strongly recommend adding ~/.cabal/bin to your $PATH, or you'll have to
invoke Haste as ~/.cabal/bin/hastec, which is sort of awkward.


Building manually
-----------------

First off, build Fursuit and Haste using cabal.

Next step, get the base libraries from http://ekblad.cc/haste-libs.tar.bz2
and untar them into your home directory; they unpack to ./.haste/ so
everything will end up in its proper place. If you want to build them yourself
instead, download the source code of a recent version of GHC (7.4.1 is the
only version tested so far) and run ./buildlibs $PATH_TO_GHC_SOURCE from within
the Haste directory.

Finally, reinstall Fursuit and Haste after installing the base libraries,
to ensure that proper code is generated for everything.


Usage
-----

To compile your Haskell program to a Javascript blob ready to be included in an
HTML document or run using a command line interpreter:

    $ hastec myprog.hs

This is equivalent to calling ghc --make myprog.hs; Main.main will be called
as soon as the JS blob has finished loading.

You can pass the same flags to hastec as you'd normally pass to GHC:

    $ hastec -O2 -fglasgow-exts myprog.hs

Haste also has its own set of command line arguments. Invoke it with --help to
read more about them.


Reactive web EDSL
-----------------

Haste comes with a basic, environment for writing client side web applications
in a reactive fashion. See Fursuit for more information.

As the reactive library relies heavily on Applicative, you may find the idiom
brackets of the Strathclyde Haskell Enhancement
(https://personal.cis.strath.ac.uk/~conor/pub/she/) quite useful.


Libraries
---------

Haste is able to use the standard Haskell libraries to a certain extent.
However, there are a few caveats. The base libraries need to be built on a 32
bit machine as Javascript stores everything as Double, which isn't enough for
64 bit integers.

Many library features also make use of native functionality that is hard or
impossible to implement on top of Javascript; the Read type class, with its
heavy use of iconv, is a prime example.


Why yet another Haskell to Javascript compiler?
-----------------------------------------------

Existing implementations either produce huge code, require a fair amount of
work to get going, or both. With Haste, the idea is to give you a drop-in
replacement for GHC that generates relatively lean code.


Known issues
------------

* No 64-bit math. Use Integer if you need large integers, use Double if you
  want as fast math as possible (yes, even for integer math.)

* Same-named modules in different packages overwrite each other when compiling
  with --libinstall.

* Not all GHC primops are implemented; if you encounter an unimplemented
  primop, I'd be happy if you'd report it together with a small test case that
  demonstrates the problem.

* Base libraries built on a 64 bit machine won't work. Don't even bother.

* Read is completely broken until a JS-native alternative can be written.

* Conversions between basic numeric types and strings work, but shouldn't
  really be used since they're doing a lot of work for something that's
  a primitive operation in JS. JS-native substitutes are available in the form
  of show_, read_ and round_ for all types which have an underlying Number
  representation with Haste (Int, Float and Double.)

* A program that throws unhandled exceptions may not always give a nice error
  message.

* Word32 produces funny results; use Word instead, which is guaranteed to be
  32 bits.
