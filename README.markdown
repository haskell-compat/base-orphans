# `base-orphans` [![Hackage version](https://img.shields.io/hackage/v/base-orphans.svg?style=flat)](http://hackage.haskell.org/package/base-orphans) [![Build Status](https://img.shields.io/travis/haskell-compat/base-orphans.svg?style=flat)](https://travis-ci.org/haskell-compat/base-orphans)

## Scope

`base-orphans` defines orphan instances that mimic instances available in later
versions of `base` to a wider (older) range of compilers. `base-orphans` does
not export anything except the orphan instances themselves and complements
[base-compat](http://hackage.haskell.org/package/base-compat).

## Usage

To use `base-orphans`, simply `import Data.Orphans ()`.

## What is covered

 * Added `Eq` and `Ord` instances for `Control.Exception.ErrorCall`
 * Added `Eq`, `Ord`, `Read`, and `Show` instances for data types in `GHC.Generics`
 * Added `Monoid`, `Eq`, `Ord`, `Read`, and `Show` instances for `Const`
 * Added `Read` and `Show` instances for `Down`
 * Added `Eq`, `Ord`, `Read`, and `Show` instances for `ZipList`
 * Added `Monad` instance for `WrappedMonad`
 * Added `Data` and `IsList` instances for `Version`
 * `Bits` instance for `Bool`
 * `Generic` instance for `All`, `Any`, `Const`, `Dual`, `Endo`, `First`, `Last`, `Product`, `Sum`, `WrappedArrow`, `WrappedMonad`, and `ZipList`
 * `Generic1` instance for `Const`, `Dual`, `First`, `Last`, `Product`, `Sum`, `WrappedArrow`, `WrappedMonad`, and `ZipList`
 * `Foldable` instance for `Either`, `(,)` and `Const`
 * `Functor` instance for `ArgOrder`, `OptDescr`, and `ArgDescr`
 * `Num` instance for `Sum` and `Product`
 * `Storable` instance for `Complex` and `Ratio`
 * `Traversable` instance for `Either`, `(,)` and `Const`
 * `Typeable` instance for most data types and typeclasses (when possible)

## Supported versions of GHC/`base`

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

Patches are welcome; add tests for new code!
