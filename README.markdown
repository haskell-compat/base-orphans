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

 * `Alternative`, `MonadPlus`, and `MonadZip` instances for `Proxy`
 * `Alternative`, `Applicative`, `Bounded`, `Data`, `Enum`, `Foldable`, `Functor`, `Ix`, `Monad`, `MonadFix`, `MonadPlus`, `MonadZip`, and `Traversable` instances for data types in `GHC.Generics`
 * `Applicative` and `Alternative` instances for `ReadP` and `ReadPrec`
 * `Applicative` instance for strict and lazy `ST`
 * `Applicative`, `Foldable`, `Functor`, `Monad`, and `Traversable` instances for `Complex`,
   `Dual`, `First`, `Last`, `Product`, and `Sum`
 * `Bits` instance for `Bool`
 * `Bits`, `Bounded`, and `Integral` instances for `CDev`
 * `Bits`, `Bounded`, `Enum`, `FiniteBits`, `Floating`, `Fractional`, `Integral`, `IsString`, `Ix`, `Num`, `Real`, `RealFloat`, `RealFrac`, and `Storable` instances for `Const` and `Identity`
 * `Data` instances for `All` and `Any`
 * `Data`, `MonadFix` and `MonadZip` instances for `Alt`, `Dual`, `First`, `Last`,
   `Product`, and `Sum`
 * `Data` and `IsList` instances for `Version`
 * `Eq` and `Ord` instances for `Control.Exception.ErrorCall`
 * `Eq`, `Ord`, `Read`, and `Show` instances for data types in `GHC.Generics`
 * `Eq`, `Ord`, `Read`, `Show`, `Foldable`, and `Traversable` instances for `ZipList`
 * `Foldable` instance for `Either`, `(,)` and `Const`
 * `Functor` instance for `Handler`, `ArgOrder`, `OptDescr`, and `ArgDescr`
 * `Functor`, `Applicative`, `Alternative`, and `MonadPlus` instances for `ArrowMonad`
 * `Monad` instance for `(,)`
 * `Monad` instance for `WrappedMonad`
 * `MonadZip` instance for `Maybe`
 * `Monoid`, `Eq`, `Ord`, `Read`, and `Show` instances for `Const`
 * `Monoid` instances for `Identity` and `IO`
 * `Num` instance for `Sum` and `Product`
 * `Read` instance for `Fixed`
 * `Read` and `Show` instances for `Down`
 * `Show` instance for `Fingerprint`
 * `Storable` instance for `()`, `Complex`, and `Ratio`
 * `Traversable` instance for `Either`, `(,)` and `Const`
 * `Typeable` instance for most data types, typeclasses, and promoted data constructors (when possible)

## What is not covered
`base-orphans` does not define the following instances:

* `Generic` or `Generic1` instances. These can be found in the
  [`Generics.Deriving.Instances`](https://hackage.haskell.org/package/generic-deriving-1.8.0/docs/Generics-Deriving-Instances.html)
  module of the [`generic-deriving`](https://hackage.haskell.org/package/generic-deriving)
  library.
* The `Alternative IO` and `MonadPlus IO` instances. These can be found in the
  [`Control.Monad.Trans.Error`](http://hackage.haskell.org/package/transformers-0.4.3.0/docs/src/Control-Monad-Trans-Error.html#line-69)
  module of the [`transformers`](http://hackage.haskell.org/package/transformers) library.

## Supported versions of GHC/`base`

 * `ghc-8.0.1`  / `base-4.9.0.0`
 * `ghc-7.10.3` / `base-4.8.2.0`
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
