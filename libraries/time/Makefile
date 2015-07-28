default: clean test install sdist

# Building

clean:
	cabal clean

configure:
	cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests

build: configure
	cabal build --ghc-options=-Werror

test: configure
	cabal test --ghc-options=-Werror --test-option=--hide-successes --test-option=--color

haddock: configure
	cabal haddock

copy: build test haddock
	cabal copy

install:
	cabal install --user --ghc-options=-Werror --enable-library-profiling --enable-executable-profiling

sdist: clean configure
	cabal sdist

# switch off intermediate file deletion
.SECONDARY:

.PHONY: default clean configure build haddock copy install test sdist
