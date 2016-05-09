# th-utilities

The 'th-utilities' package provides a number of useful utilities for
[Template Haskell](https://hackage.haskell.org/package/template-haskell-2.10.0.0).
In particular:

* `TH.Derive` provides a convenient system for using TH to derive typeclass
  instances.  It allows for open registration of TH derivers, and reuses
  instance syntax for invoking them.

* `TH.ReifyDataType` provides utilities for reifying simplified datatype info.
  It omits details that you don't usually want to handle, making it much more
  straightforward to generate code based on datatype structure.

* `TH.RelativePaths` provides utilities for loading files based on paths
  relative to the cabal file. This is particularly handy for loading code into
  ghci even when its current dir isn't the package dir.  Ideally, this module
  would be used by everyone who currently uses `qAddDependentFile`.

* `TH.Utilities` provides a miscellaneous set of utilities that are useful
  within this package and elsewhere.
