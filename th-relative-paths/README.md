# th-relative-paths

This package provides Template Haskell utilities for loading files based on paths
relative to the root of your Cabal package.

Normally when building a cabal package, GHC is run with its current directory
set at the package's root directory. This allows using relative paths to refer
to files. However, this becomes problematic when you want to load modules from
multiple projects, such as when using "stack ghci".

This solves the problem by getting the current module's filepath from TH via
'location'. It then searches upwards in the directory tree for a .cabal file,
and makes the provided path relative to the folder it's in.
