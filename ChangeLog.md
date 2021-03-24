# ChangeLog

## 0.2.4.2

* Fixes compilation with `GHC-9.0.*`.  See [#14][]

[#14]: https://github.com/fpco/th-utilities/issues/14

## 0.2.4.1

* Fixes generated Storable instances to have a `sizeOf` definition
  which works with `-XStrict`. See [#13][]

[#13]: https://github.com/fpco/th-utilities/issues/13

## 0.2.4.0

* Compatibility with GHC-8.10

* Behavior change in reification of type family instances. Instead of
  erroring if the instance mentions a kind variable, now just ignores
  it.

## 0.2.3.1

* Compatibility with GHC-8.8

## 0.2.3.0

* Improved fix to the type variable behavior with GHC <= 7.10.  Uses
  `Any` in place of type variables instead of `()`, to allow for more
  kinds than just `*` and `Constraint`.

## 0.2.2.0

* Fixes derive and instantiator mechanisms to work with ghc 7.10 and
  earlier.  Previously, invocation was broken when type variables were
  used.

* Fixes `freeVarsT` - it now looks through more constructors of `Type`.

* Adds `dequalifyTyVars` to dequalify every type variable.

## 0.2.0.1

* Fixes build on 7.8

* Fixes warnings

## 0.2.0.0

* Adds TH.ReifySimple, which supports reifying most of the information TH users
  care about.

* Adds some utilities based on SYB, which is often useful for TH.

* Makes relative path stuff less noisyi with GHCi.

## 0.1.0.0

* First public release
