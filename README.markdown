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

## About `base-orphans-0`

`base-orphans-0` is a special release that intentionally does not export any
modules. `base-orphans-0` is used when retroactively adding `base-orphans`
dependencies to older Hackage libraries to ensure that they cannot be built in
combination with more recent versions of `base-orphans` that would cause them
to break. (For example, if a package defines an orphan instance which clashes
with one in `base-orphans`.)

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
