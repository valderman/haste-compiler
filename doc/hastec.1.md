% HASTEC(1) Haste User Manual
% Anton Ekblad
% October 29, 2015
NAME
===
hastec - compile Haskell programs into JavaScript


SYNOPSIS
========
**hastec** [ *option* | *filename* ]...


DESCRIPTION
===========
This manual page documents briefly the **hastec** command.
In addition to the options listed here, **hastec**
also accepts all the same command line options as the GHC compiler,
with the exception of **-prof** which is silently ignored.

By default, **hastec** behaves as though it was called with GHC's **--make**
option, compiling and linking a complete JavaScript program which,
when included into a web page, sets the program's *main*
function to execute when the page has finished loading.


OPTIONS
=======
Options are processed from left to right. If two conflicting options are given,
the latter will take precedence. This means that you can use options such as
**--opt-all** which imply multiple other options, and selectively disable any
implied options that you don't want. For instance, the command line
**hastec --opt-all --opt-tail-loop-transform=off**
will enable all safe optimizations except for the tail loop transformation.


Linking
-------

\--dont-link
:   Don't link generated .jsmod files into a .js blob.

--link-jslib[=*FILE*]
:   Create a jslib file instead of an executable. If no *FILE* is given, the
    package key of the library currently being compiled is used as the library
    file name.

--with-js=*FILES*
:   Link the given comma-separated list of JavaScript files into the final
    JavaScript file.


Program startup
---------------

--onexec
:   Launch application immediately when the JavaScript file is loaded.
    Shorthand for **--start=onexec**.

--onload
:   Launch application on window.onload. Shorthand for **--start=onload**.
    This is the default behavior.

--start=*CODE*
:   Specify custom start code. '\$HASTE_MAIN' will be replaced with the
    application's main function. For instance,
    **--start='$("foo").onclick(\$HASTE_MAIN);'**
    will use jQuery to launch the application whenever the element with the
    id "foo" is clicked.
    
    In addition to custom JavaScript code, *onexec* and *onload* are also
    acceptable values for *CODE*, indicating that the program is to be
    executed as soon as parsing finishes or after the page has finished
    loading respectively.


Output control
--------------

-o *FILE*, --out=*FILE*
:   Write JavaScript output to *FILE*.

--outdir=*DIR*
:   Write intermediate files to *DIR*.

--output-html
:   Write the JavaScript output to an HTML file with a simple HTML skeleton.

--own-namespace
:   Wrap the whole program in a closure to avoid polluting the global namespace.
    Incurs a performance hit, and makes minification slightly less effective.


Code generation
---------------

--full-unicode
:   Enable full generalCategory Unicode support. May bloat output by upwards of
    150 KB.

--no-use-strict
:   Do not emit **"use strict";** declaration. Does not affect minifier
    behavior, but does affect any external JavaScript included
    using **--with-js**.

--overwrite-scrutinees
:   Overwrite scrutinees when evaluated rather than allocating a new local for
    the evaluated value. This is largely experimental.

--output-jsflow
:   Output code for use with the JSFlow interpreter. Note that this may leave
    your code crippled, since JSFlow doesn't support all of Haste's needs.


Debugging
---------

--annotate-externals
:   Annotate all JavaScript-native symbols and inline JavaScript in
    generated code with **/\* EXTERNAL \*/**.

--annotate-symbols
:   Annotate all non-external, non-local symbols with their qualified
    Haskell names.

--debug
:   Output annotated, pretty-printed JavaScript code. Equivalent to
    **--annotate-externals --annotate-symbols --pretty-print**.

--ddisable-js-opts
:   Disable any and all optimizations over the resulting JavaScript code.
    Note that this disables tail call elimination, possibly changing the
    semantics of programs that rely on it.

--dtrace-primops
:   Print all calls to primitive operations, together with their arguments and
    return values. Not really useful unless Haste was booted with primop
    tracing enabled.

--preserve-names
:   Preserve Haskell names in JavaScript code as far as possible.
    Highly experimental and may break your code.

--pretty-print
:   Print JavaScript output using whitespace and indentation.


Optimization
------------

Many optimization options take an optional *on*/*off* argument.
Passing an optimization option without this argument turns the optimization
*on*. For instance, **--opt-minify** is equivalent to **--opt-minify=on**.

--opt-all
:   Enable all safe optimizations except minification. Individual optimizations
    may be turned off using their individual flags.

--opt-detrampoline-threshold=*N*
:   Remove trampolining and tail calls for provably finite tail call chains
    shorter than *N* calls. Set to *0* to disable. A value of *N=3* is implied
    by **--opt-all**.

--opt-flow-analysis[=*on*|*off*]
:   Enable whole program flow analysis. Highly experimental and possibly slow
    and/or incorrect. Don't use for now.

--opt-inline-ffi-primitives[=*on*|*off*]
:   Inline FFI call primitives where possible.

--opt-minify[=*on*|*off*]
:   Minify JavaScript output using Google Closure compiler.

--opt-minify-flag=*FLAG*
:   Pass *FLAG* to Closure. To minify programs in strict mode, use
    **--opt-minify-flag='--language_in=ECMASCRIPT5_STRICT'**.

--opt-tail-chain-bound=*N*
:   Bound tail call chains at *N* stack frames.
    By default, tail call chains are not allowed to grow the call stack at all.
    This is very inefficient, however, so ideally one wants to strike a balance
    between amortizing the cost of the trampolining machinery and the size of
    the call stack. A value between 10 and 100 is usually a good choice.

--opt-tail-loop-transform[=*on*|*off*]
:   Optimize tail recursive functions into loops when possible.
    Enabled by default.

--opt-unsafe
:   Enable all optimizations, safe and unsafe. Equivalent to
    **--opt-all --opt-unsafe-ints**.

--opt-unsafe-ints
:   Enable unsafe Int arithmetic.
    Equivalent to **--opt-unsafe-mult --opt-vague-ints**.

--opt-unsafe-mult[=*on*|*off*]
:   Use JavaScript's built-in multiplication operator for fixed precision
    integer multiplication. This may speed up Int multiplication by a factor
    of at least four, but may give incorrect results when the product falls
    outside the interval [-2^52, 2^52]. In browsers which support Math.imul,
    this optimization will likely be slower than the default.

--opt-vague-ints[=*on*|*off*]
:   Int math has 53 bits of precision, but gives incorrect results rather than
    properly wrapping around when those 53 bits are exceeded. Bitwise operations
    still only work on the lowest 32 bits.

--opt-whole-program[=*on*|*off*]
:   Perform optimizations over the whole program during linking. May
    significantly increase link time.


Misc. options
-------------

-?, --help
:   Display help message.

-v, --verbose
:   Display even the most obnoxious warnings and messages.
