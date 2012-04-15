Haste
=====

A compiler to generate Javascript code from Haskell.


Building
--------

Use cabal:

    $ runghc Setup.hs --user configure
    $ runghc Setup.hs build
    $ runghc Setup.hs install


Usage
-----

To compile your Haskell program to a Javascript blob ready to be included in an
HTML document or run using a command line interpreter:

    $ hastec myprog.hs

This is equivalent to calling ghc --make myprog.hs; Main.main will be called
as soon as the JS blob has finished loading.

You can pass the same flags to hastec as you'd normally pass to GHC:

    $ hastec -O2 -fglasgow-exts myprog.hs

haste also has its own set of command line arguments. Invoke it with --help to
read more about them.

Reactive web EDSL
-----------------

Haste comes with an (as of yet) incomplete, very basic, environment for writing
client side web applications in a reactive fashion. See the included modules
Haste.Reactive and Haste.Reactive.DOM for more details.

As the reactive library relies heavily on Applicative, you may find the idiom
brackets of the Strathclyde Haskell Enhancement
(https://personal.cis.strath.ac.uk/~conor/pub/she/) quite useful.


Libraries
---------

Haste is able to use the standard Haskell libraries to a certain extent.
However, there are a few caveats. The libraries need to be built on a 32 bit
machine as Javascript stores everything as Double, which isn't enough for 64
bit integers.

The libraries also need to be built with the same options as
the "normal" libraries you use when compiling your applications for desktop
use, or GHC will have a great time inlining symbols that don't exist into
your programs.

Finally, many library features make use of native functionality that is hard
or impossible to implement on top of Javascript.

With these restrictions, building the standard libraries is quite cumbersome;
the proper solution in the long run is probably to reimplement the parts of
ghc-prim and base that make sense in a web context. In the meanwhile, however,
here's how you build them on a *nix system:

1. download the source for the GHC version you're currently running;

2. unpack it, copy mk/build.mk.sample to mk/build.mk;

3. at the top of mk/build.mk, add INTEGER_LIBRARY = integer-simple;

4. uncomment the line that says BuildFlavour = quick;

5. build the whole thing using ./configure && make as usual;

6. run the buildlibs script in the root of the haste-compiler directory, with
   the path to where you unpacked the GHC source as its only argument;

7. You're hopefully done.

If that's too much hassle, I've prepared a tarball containing the relevant
bits of ghc-prim, integer-simple, base and containers. It's built with -O2
using GHC 7.4.1 on a 32 bit Debian unstable box; the closer your GHC install
is to that the greater the chance that they will work for you.

The tarball unpacks to ./.haste, so if you unpack it in your home directory
everything should turn up in its proper place.

Get it from: http://ekblad.cc/haste-libs.tar.bz2


Why yet another Haskell to Javascript compiler?
-----------------------------------------------

Existing implementations either produce huge code, require a fair amount of
work to get going, or both. With haste, the idea is to give you a drop-in
replacement for GHC that generates relatively lean code.


Known issues
------------

* Doesn't yet do tail call elimination.

* Same-named modules in different packages overwrite each other when compiling
  with --libinstall.

* Not all GHC primops are implemented; if you encounter an unimplemented
  primop, I'd be happy if you'd report it together with a small test case that
  demonstrates the problem.

* Base libraries built on a 64 bit machine won't work. Don't even bother.

* Read and Show are currently broken for Float and Double. JS-native
  substitutes are available in the form of show_ and read_ for all types which
  have an underlying Number representation with Haste (Int, Float and Double.)
  Apart from actually working for Float and Double, they're also quite a lot
  faster for Ints. Additionally, round is broken for Float and Double as well,
  and there's a round_ for those types that not only works but which is also
  much faster than Prelude's round would have been had it worked.

* A program that has the value of _|_ may not always give a nice error
  message.
