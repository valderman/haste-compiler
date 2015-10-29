% HASTEC(1) Haste User Manual
% Anton Ekblad
% October 29, 2015

NAME
====
haste-cat - inspect Haste intermediate code files


SYNOPSIS
========
**haste-cat** package-id:module-name


DESCRIPTION
===========
**haste-cat** prints the intermediate Haste code for the given Haskell module
to standard output.

Package identifiers must be given with version numbers and package keys
(e.g. haste-prim-0.5.1.0-BtuKn3ytxKq6eTkR391Vdt as opposed to just haste-prim),
to avoid ambiguity in the face of multiple installed
versions of the same package.
GHC 7.8-based builds don't use package keys and so should be omitted.

See **haste-pkg list** for a list of installed packages and their corresponding
package keys.

If a package exists in more than one location, only the one first encountered
will be printed. **haste-cat** searches the following paths for installed
packages, in this order.

1. The current working directory
2. Haste's user library directory
3. Haste's system library directory
   (where applicable; only portable installs have a separate system directory)
