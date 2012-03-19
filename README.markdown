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

To compile your Haskell program to a Javascript ready to be included in a HTML
document or run using a command line interpreter:

    $ hastec myprog.hs

This is equivalent to calling ghc --make myprog.hs; Main.main will be called
as soon as the JS blob has finished loading.

You can pass the same flags to hastec as you'd normally pass to GHC:

    $ hastec -O2 -fglasgow-exts myprog.hs

haste also has its own set of command line arguments. Invoke it with --help to
read more about them.


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

* There may be issues with Ints and Integers larger than 2^31 with libraries
  built on a 64 bit machine, as Ints are represented by the JS Number type.
  Int64 is also unlikely to work well if your program or the base libraries
  are built on a 64 bit machine.

* The Read class and conversions (show and round, mainly) for floating point
  types depend on lots of low level pointer arithmetic magic and other tricks
  that make just as little sense in a Javascript setting, and so are broken.
  Primitive substitutes are provided in the form of round_, show_ and read_.

* A program that has the value of _|_ may not always give a nice error
  message.
