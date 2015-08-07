## Changes in next
 - `Fix` build on OSes where `HTYPE_DEV_T = Int32`

## Changes in 0.4.2
 - `Functor` instances for `Handler`
 - `Functor`. `Applicative`, `Alternative`, and `MonadPlus` instances for
   `ArrowMonad`
 - Expose `Read` and `Show` instances for `Down` on GHCs before 7.8
 - `Bits`, `Bounded`, and `Integral` instances for `CDev`

## Changes in 0.4.1
 - Fixed imports on GHC < 7.8 on Windows

## Changes in 0.4.0
 - Removed all `Generic` and `Generic1` instances. These have been moved to the
   `generic-deriving` library.

## Changes in 0.3.3
 - `Typeable` instances for `(~)`, `Any`, `Constraint`, `CSigset`, `Handler`,
   `Opaque`, `SPEC`, and every promotable data constructor in `base`

## Changes in 0.3.2
 - `Storable (Complex a)` instance no longer requires a `RealFloat a`
   constraint if using `base-4.4` or later

## Changes in 0.3.1
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
