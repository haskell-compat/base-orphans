## Changes in next
 - `Functor`, `Applicative`, and `Monad` instances for `First` and `Last`

## Changes in 0.3.0
 - `Show` instance for `Fingerprint`
 - `Data.Orphans` is now `Trustworthy`
 - Backported the `Generic` and `Generic1` instances available in `base-4.7.0.0`
   to GHC 7.2, 7.4, and 7.6, namely
   * `Const`, `WrappedMonad`, and `ZipList` from `Control.Applicative`
   * `WrappedArrow` from `Control.Category`
   * `All`, `Any`, `Dual`, `Endo`, `First`, `Last`, `Product`, and `Sum` from
     `Data.Monoid`
   * `U1`, `Par1`, `Rec1`, `K1`, `M1`, `(:+:)`, `(:*:)`, `(:.:)`, `Arity`,
     `Associativity`, and `Fixity` from `GHC.Generics`

## Changes in 0.2.0
 - Drop GHC 6.12 (and `base-4.2.0.0`) compatibility
 - Fix Windows, GHCJS build
 - `Read` instance for `Fixed`
 - `Applicative` instances for strict and lazy `ST`
 - `Typeable` instance for `SampleVar`
 - `Applicative` and `Alternative` instances for `ReadP` and `ReadPrec`
 - `Typeable` instance for `KProxy`
 - `Typeable` instances for more data types in `GHC.`-prefixed modules
 - `Generic` instances for `Arity`, `Associativity`, and `Fixity` from
   the `GHC.Generics` module
 - Corrected the `Generic` instance for `(:*:)` to work around GHC bug #9830
