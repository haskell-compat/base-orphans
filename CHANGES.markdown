## Changes in 0.9.1 [2023.10.11]
 - Backport new instances from GHC 9.8/`base-4.19`:
   * `Eq` and `Ord` instances for `SChar`, `SNat`, and `SSymbol`
   * `Enum`, `Bounded`, `Num`, `Real`, and `Integral` instances for `Compose`

## Changes in 0.9.0 [2023.03.05]
 - Adapt to recent changes to `Down` instances:
   * The `Bounded` instance for `Down` was changed in `base-4.15.0.0` to swap
     the values of `minBound` and `maxBound` for the underlying type. This
     change has now been propagated to `base-orphans`.
   * The `Enum` instance for `Down` was removed in `base-4.15.0.0`, but a
     different version of the instance was added back in `base-4.18.0.0`, where
     `succ` and `pred` are swapped. We have changed the backported version of
     this instance in `base-orphans` to match the behavior of the instance
     added in `base-4.18.0.0`.
   * The `Integral` instance for `Down` was removed from `base` entirely in
     `base-4.15.0.0`. We have finally removed it from `base-orphans` in this
     release, as it actively makes it more difficult to define the
     aforementioned `Enum` instance.

## Changes in 0.8.8.1 [2023.03.05]
 - Fix GHC 9.4 build error that was accidentally introduced in
   `base-orphans-0.8.8`.

## Changes in 0.8.8 [2023.03.05]
 - Backport new instances from GHC 9.6.1/`base-4.18.0.0`:
   * `Functor` instances for the `(,,,,) a b c d`, `(,,,,,) a b c d e`, and
     `(,,,,,) a b c d e f` tuple types.
   * `Eq` and `Ord` instances for `Generically1`

## Changes in 0.8.7 [2022.08.11]
 - Backport new instances from GHC 9.2.2/`base-4.16.1.0`:
   * `Ix` instances for various integral types in `Foreign.C.Types`,
     `Foreign.Ptr`, and `System.Posix.Types`. For a complete list of such types,
     see https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7385.
 - Backport a bugfix for the `Ord1 Down` instance from GHC 9.4/`base-4.17` such
   that it now uses reverse sort order.

## Changes in 0.8.6 [2021.10.29]
 - Backport `Eq`, `Ord`, `Bounded`, `Enum`, and `Ix` instances for `Solo`,
   introduced in GHC 9.2/`base-4.16`
 - Remove the backported `Eq1` and `Ord1` instances for `Fixed` that were
   introduced in `base-orphans-0.8.5`. While these briefly appeared in a
   release candidate version of GHC 9.2.1, they were ultimately removed from
   the final 9.2.1 release. This release of `base-orphans` synchronizes with
   that change.

## Changes in 0.8.5 [2021.08.29]
 - Backport new instances from GHC 9.2/`base-4.16`
   * `Eq1`, `Read1`, and `Show1` instances for `Complex`
   * `Eq1` and `Ord1` instances for `Fixed`
   * `Eq{1,2}`, `Ord{1,2}`, `Show{1,2}`, and `Read{1,2}` instances for `(,,)`
      and `(,,,)`
   * `Semigroup` and `Monoid` instances for `Data.Functor.Product` and
     `Data.Functor.Compose`

## Changes in 0.8.4 [2020.12.09]
 - Backport the `Ord` instances for `TyCon` and `TypeRep` to be available on
   GHC 7.0.

## Changes in 0.8.3 [2020.09.30]
 - Backport new instances from GHC 9.0/`base-4.15`
   * `MonadFix` and `MonadZip` instances for `Complex`
   * `Ix` instances for tuples of size 6 through 15

## Changes in 0.8.2 [2020.01.27]
 - Backport new instances from GHC 8.10/`base-4.14`
   * `Bits`, `Bounded`, `Enum`, `FiniteBits`, `Floating`, `Fractional`,
     `Integral`, `Ix`, `Real`, `RealFrac`, `RealFloat` and `Storable` instances
     for `Data.Ord.Down`
   * `Functor`, `Applicative`, `Monad`, `Alternative`, and `MonadPlus` instances
     for `Kleisli`
   * `Functor`, `Applicative`, and `Monad` instances for
     `(,,) a b` and `(,,,) a b c`
   * `Data` instances for `WrappedArrow` and `WrappedMonad`
   * `Data` and `IsList` instances for `ZipList`
   * `TestEquality` instance for `Compose`
 - Backport the `Typeable (() :: Constraint)` instance to be available on
   GHC 7.8.

## Changes in 0.8.1 [2019.03.25]
 - Ensure that the test suite passes non-negative numbers to `Bits` methods
   that require them, such as `bit`, `setBit`, `clearBit`, `complementBit`,
   and `testBit`.

## Changes in 0.8 [2018.07.02]
 - Backported new instance from GHC 8.6/`base-4.12`
   * Data types in `GHC.Generics`:
     - `Applicative` instance for `K1`
     - `Semigroup` instances for `U1`, `Par1`, `Rec1`, `K1`, `M1`, `(:*:)`,     `(:.:)`, and `V1`
     - `Monoid`    instances for `U1`, `Par1`, `Rec1`, `K1`, `M1`, `(:*:)`, and `(:.:)`
   * `Foldable` and `Traversable` instances for `Data.Monoid.Alt`
   * `MonadFix`, `MonadZip`, `Data`, `Foldable`, `Traversable`, `Eq1`, `Ord1`,
     `Read1`, and `Show1` instances for `Data.Ord.Down`

## Changes in 0.7 [2018.03.08]
 - Backported new instances from GHC 8.4/`base-4.11`
   * `Alternative` instance for `ZipList`
   * `Data` instance for `IntPtr` and `WordPtr`
   * `Num`, `Functor`, `Applicative`, `Monad`, `Semigroup`, and `Monoid`
     instances for `Data.Ord.Down`
   * `MonadFail`, `Semigroup`, and `Monoid` instances for strict `ST`

## Changes in 0.6 [2017.04.10]
 - Backported new instances from GHC 8.2/`base-4.10`
   (see https://github.com/haskell-compat/base-orphans/issues/39):
   * `Data` instance for `Const`
   * `Eq1`, `Ord1`, `Read1`, and `Show1` instances for `NonEmpty`
   * `Semigroup` instances for `IO`, `Event`, and `Lifetime`
 - Backported `Typeable` instances for `(:+:)`, `(:*:)`, `(:.:)`, `M1`, `Rec1`,
   `ArrowMonad`, `Kleisli`, `WrappedArrow`, `WrappedMonad`, and `Any` on GHC
   7.6 and earlier
 - Backported `Data` instances for `(:+:)`, `(:*:)`, `(:.:)`, `M1`, and `Rec1`
   on GHC 7.6 and earlier

## Changes in 0.5.4
 - Backported `Bits`, `FiniteBits`, `Floating`, `Fractional`, `Integral`,
   `IsString`, `Num`, `Real`, `RealFloat`, and `RealFrac` instances for
   `Identity` and `Const` (introduced in `base-4.9`)

## Changes in 0.5.3
 - Backported `Alternative`, `MonadPlus` and `MonadZip` instances for `U1` and
   `Proxy`, and made the `Functor`, `Foldable`, `Traversable`, `Alternative`,
   and `Monad` instances for `U1` lazier to correspond with `base-4.9`

## Changes in 0.5.2
 - Backported `Enum`, `Bounded`, `Ix`, `Functor`, `Applicative`, `Monad`,
   `MonadFix`, `MonadPlus`, `MonadZip`, `Foldable`, `Traversable`, and
   `Data` instances for datatypes in the `GHC.Generics` module (introduced in
   `base-4.9`)

## Changes in 0.5.1
 - The `Storable` instances for `Complex` and `Ratio` are now exactly as lazy
   as their counterparts in `base` (see issue
   [#36](https://github.com/haskell-compat/base-orphans/issues/36))

## Changes in 0.5.0
 - GHC 8.0 compatibility
 - Backported instances introduced in GHC 8.0/`base-4.9`
   (see https://github.com/haskell-compat/base-orphans/issues/32)

## Changes in 0.4.5
 - Import `Control.Monad.Instances` (which exports `Functor` and `Monad`
   instances for `(->) r`, and `Functor` instances for `(,) a` and `Either a`)
   on GHCs before 7.6. This ensures that these instances will always be in
   scope, and you won't have to import a module which is deprecated on recent
   GHC releases.
 - Fix build on GHC HEAD (again)

## Changes in 0.4.4
 - Fix build on GHC HEAD

## Changes in 0.4.3
 - Fix build on OSes where `HTYPE_DEV_T = Int32` (e.g., OS X)

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
