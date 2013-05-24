Haste
=====

A compiler to generate Javascript code from Haskell.


Building
--------

First, make sure that you are running GHC 7.6.x. Pre-7.6 versions will refuse
to compile due to changes in GHC. The libraries fetched by haste-boot were
built using 7.6.3, but should work fine with any GHC version that can build
Haste.

Second, clone haste-compiler and install it using `cabal install`.

Now run `./buildlibs.sh` to build the standard libraries. GHC sources are no
longer needed.

You should probably run the test suite first though, to verify that everything
is working. To do that, execute `./runtests.sh` in the Haste root directory.
You may also run only a particular test by executing `./runtest.sh NameOfTest`.
The test suite is written for use with Mozilla's SpiderMonkey Javascript engine,
which can be found in the `spidermonkey-bin` package if you're running Debian or
any of its derivatives, and may or may not work with other implementations.


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
read more about them. In particular --opt-all, --opt-google-closure and
--with-js should be fairly interesting.

If you want your package to compile with both Haste and, say, GHC, you might
want to use the CPP extension for conditional compilation. Haste defines the
preprocessor symbol `__HASTE__` in all modules it compiles.

Haste also comes with wrappers for cabal and ghc-pkg, named haste-inst and
haste-pkg respectively. You can use them to install packages just as you would
with vanilla GHC and cabal:

    haste-inst install mtl

This will only work for libraries, however, as installing Javascript
"executables" on your system doesn't make much sense. You can still use
`haste-inst build` to build your "executables" locally, however.

Finally, you can interact with Javascript code using the FFI. See
`doc/js-externals.txt` for more information about that.

For more information on how Haste works, see
[the Haste Report](http://ekblad.cc/hastereport.pdf "Haste Report"),
though beware that parts of Haste may have changed quite a bit.


Reactive web EDSL
-----------------

Haste comes with a basic, environment for writing client side web applications
in a reactive fashion. See Fursuit for more information.

As the reactive library relies heavily on Applicative, you may find the idiom
brackets of the Strathclyde Haskell Enhancement
(https://personal.cis.strath.ac.uk/~conor/pub/she/) quite useful.


Libraries
---------

Haste is able to use standard Haskell libraries to a certain extent.
However, many primitive operations are still not implemented, which means that
any code making use of them will give you a compiler warning, then die at
runtime with an angry error. This is currently being worked on.


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

* A program that throws unhandled exceptions may not always give a nice error
  message.
