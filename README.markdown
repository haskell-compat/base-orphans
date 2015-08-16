# `base-orphans`
[![Hackage](https://img.shields.io/hackage/v/base-orphans.svg)][Hackage: base-orphans]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/base-orphans.svg)](http://packdeps.haskellers.com/reverse/base-orphans)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-MIT-brightgreen.svg)][tl;dr Legal: MIT]
[![Build](https://img.shields.io/travis/haskell-compat/base-orphans.svg)](https://travis-ci.org/haskell-compat/base-orphans)

[Hackage: base-orphans]:
  http://hackage.haskell.org/package/base-orphans
  "base-orphans package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: MIT]:
  https://tldrlegal.com/license/mit-license
  "MIT License"

## Scope

`base-orphans` defines orphan instances that mimic instances available in later
versions of `base` to a wider (older) range of compilers. `base-orphans` does
not export anything except the orphan instances themselves and complements
[base-compat](http://hackage.haskell.org/package/base-compat).

Note that `base-orphans` doesn't cover every possible instance. See the
[What is not covered](#what-is-not-covered) section for exceptions.

## Usage

To use `base-orphans`, simply `import Data.Orphans ()`.

## What is covered

 * Added `Applicative` and `Alternative` instances for `ReadP` and `ReadPrec`
 * Added `Bits`, `Bounded`, and `Integral` instances for `CDev`
 * Added `Eq` and `Ord` instances for `Control.Exception.ErrorCall`
 * Added `Eq`, `Ord`, `Read`, and `Show` instances for data types in `GHC.Generics`
 * Added `Functor`, `Applicative`, `Alternative`, and `MonadPlus` instances for `ArrowMonad`
 * Added `Functor`, `Applicative`, and `Monad` instances for `First` and `Last`
 * Added `Monoid`, `Eq`, `Ord`, `Read`, and `Show` instances for `Const`
 * Added `Read` and `Show` instances for `Down`
 * Added `Eq`, `Ord`, `Read`, and `Show` instances for `ZipList`
 * Added `Monad` instance for `WrappedMonad`
 * Added `Data` and `IsList` instances for `Version`
 * `Applicative` instance for strict and lazy `ST`
 * `Bits` instance for `Bool`
 * `Foldable` instance for `Either`, `(,)` and `Const`
 * `Functor` instance for `Handler`, `ArgOrder`, `OptDescr`, and `ArgDescr`
 * `Num` instance for `Sum` and `Product`
 * `Read` instance for `Fixed`
 * `Show` instance for `Fingerprint`
 * `Storable` instance for `Complex` and `Ratio`
 * `Traversable` instance for `Either`, `(,)` and `Const`
 * `Typeable` instance for most data types, typeclasses, and promoted data constructors (when possible)

## What is not covered
`base-orphans` does not define the following instances:

* `Generic` or `Generic1` instances. These can be found in the
  [`Generics.Deriving.Instances`](https://hackage.haskell.org/package/generic-deriving-1.8.0/docs/Generics-Deriving-Instances.html)
  module of the [`generic-deriving`](https://hackage.haskell.org/package/generic-deriving)
  library.

## Supported versions of GHC/`base`

 * `ghc-7.10.2` / `base-4.8.1.0`
 * `ghc-7.10.1` / `base-4.8.0.0`
 * `ghc-7.8.4`  / `base-4.7.0.2`
 * `ghc-7.8.3`  / `base-4.7.0.1`
 * `ghc-7.8.2`  / `base-4.7.0.0`
 * `ghc-7.8.1`  / `base-4.7.0.0`
 * `ghc-7.6.3`  / `base-4.6.0.1`
 * `ghc-7.6.2`  / `base-4.6.0.1`
 * `ghc-7.6.1`  / `base-4.6.0.0`
 * `ghc-7.4.2`  / `base-4.5.1.0`
 * `ghc-7.4.1`  / `base-4.5.0.0`
 * `ghc-7.2.2`  / `base-4.4.1.0`
 * `ghc-7.2.1`  / `base-4.4.0.0`
 * `ghc-7.0.4`  / `base-4.3.1.0`
 * `ghc-7.0.3`  / `base-4.3.1.0`
 * `ghc-7.0.2`  / `base-4.3.1.0`
 * `ghc-7.0.1`  / `base-4.3.0.0`

We also make an attempt to keep `base-orphans` building with GHC HEAD, but due
to its volatility, it may not work at any given point in time. If it doesn't,
please report it!

Patches are welcome; add tests for new code!
