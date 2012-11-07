Haste
=====

A compiler to generate Javascript code from Haskell.


Building
--------

First, make sure that you are running GHC 7.4.2, as this is what the base
libraries installed by haste-boot expects. Other versions may work, but they
may also be quite buggy.

Second, clone haste-compiler and install it using `cabal install`.

Finally, run `haste-boot` (or `~/.cabal/bin/haste-boot`, if you don't have
`~/.cabal/bin` in your `$PATH`) and wait for it to finish - you should now be
able to use Haste!



Building your own base libraries
--------------------------------

First off, run `haste-boot --force --no-base`.
Then, download the source code of the same GHC version you're running
(7.4.1 and 7.4.2 are the only versions tested so far, but any reasonable recent
version should be OK).

Unpack the GHC sources you just downloaded, go to the directory where you
unpacked it and copy the file `mk/build.mk.sample` to `mk/build.mk` and edit
it to add the following at the very top:

    INTEGER_LIBRARY = integer-simple
    BuildFlavour = quick

Now, run `./configure && make`; wait. Fortunately, you only need to do this step
once. If you want to rebuild your libraries at any time after this, you can
start from the `buildlibs.sh` step.

After GHC has finished building, go back to the haste-compiler directory and
run `./buildlibs.sh /wherever/you/unpacked/GHC`. You may get a _LONG_ error
message from GHC while running buildlibs.sh. If you do, let it finish and
re-run it. It's also probably a good idea to reinstall Fursuit and haste-lib
after rebuilding the base libraries; do this by issuing `haste-inst install`
in their respective directories, or by running
`haste-boot --force --no-closure --no-base`.

Keep in mind, however, that a base library built on a 64-bit machine will be
broken, in particular with regards to the arbitrary precision Integer type.


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
preprocessor symbol `__HASTE__` for all modules it compiles, and `__HASTE_TCE__`
for those compiled with full trampolining tail call elimination.

Haste also comes with wrappers for cabal and ghc-pkg, named haste-inst and
haste-pkg respectively. You can use them to install packages just as you would
with vanilla GHC and cabal:

    haste-inst install mtl

This will only work for libraries, however, as installing Javascript
"executables" on your system doesn't make much sense. You can still use
`haste-inst build` to build your "executables" locally, however.

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

* Word32 produces funny results on some machines; use Word instead, which is
  guaranteed to be 32 bits with Haste.

* Haste currently uses package information for `rts`, `integer-gmp`, `ghc-prim`
  and `base` from your vanilla GHC install. As a consequence, Haste needs to be
  rebooted whenever you upgrade your GHC, and the intermediate files for those
  four need to be compiled with whatever GHC version you currently have
  installed.
