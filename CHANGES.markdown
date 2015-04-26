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
