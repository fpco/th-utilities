# th-utilities

[![Build Status](https://travis-ci.org/commercialhaskell/th-utilities.svg?branch=master)](https://travis-ci.org/fpco/th-utilities)

The 'th-utilities' package provides a number of useful utilities for
[Template Haskell](https://hackage.haskell.org/package/template-haskell-2.10.0.0).
In particular:

* [`TH.Derive`](https://github.com/fpco/th-utilities/blob/master/src/TH/Derive.hs)
  provides a convenient system for using TH to derive typeclass instances. It
  allows for open registration of TH derivers, and reuses instance syntax for
  invoking them.

  - [`TH.Derive.Storable`](https://github.com/fpco/th-utilities/blob/master/src/TH/Derive/Storable.hs)
    defines derivation of Storable for ADTs.

* [`TH.ReifyDataType`](https://github.com/fpco/th-utilities/blob/master/src/TH/ReifyDataType.hs)
  provides utilities for reifying simplified datatype info. It omits details
  that you don't usually want to handle, making it much more straightforward to
  generate code based on datatype structure.

* [`TH.RelativePaths`](https://github.com/fpco/th-utilities/blob/master/src/TH/RelativePaths.hs)
  provides utilities for loading files based on paths relative to the cabal
  file. This is particularly handy for loading code into ghci even when its
  current dir isn't the package dir. Ideally, this module would be used by
  everyone who currently uses `qAddDependentFile`.

* [`TH.Utilities`](https://github.com/fpco/th-utilities/blob/master/src/TH/Utilities.hs)
  provides a miscellaneous set of utilities that are useful within this package
  and elsewhere.
