Fursuit
=======

FUnctional Reactivity with Signals Using a sIngle Thread of control.

This library provides a simplified take on FRP, deliberately avoiding all
concurrency primitives in order to make it work with haste-compiler. It's also
quite usable in a more normal Haskell environment, though it needs some
locking to make it thread safe.
