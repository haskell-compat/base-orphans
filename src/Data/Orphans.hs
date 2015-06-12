{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NullaryTypeClasses #-}
#endif

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Exports orphan instances that mimic instances available in later versions of @base@.
To use them, simply @import Data.Orphans ()@.
-}
module Data.Orphans () where

#if MIN_VERSION_base(4,4,0) && !(MIN_VERSION_base(4,7,0))
import Data.Word (Word64)
import Numeric (showHex)
#endif

#if !(MIN_VERSION_base(4,4,0))
import Control.Concurrent.SampleVar
import Control.Monad.ST as Strict
import Data.List
#endif

#if !(MIN_VERSION_base(4,4,0)) || (__GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710)
import Data.Fixed
#endif

#if MIN_VERSION_base(4,4,0) && __GLASGOW_HASKELL__ < 710
import GHC.Fingerprint
import GHC.IO.Encoding.Failure

# if !defined(mingw32_HOST_OS) && !defined(__GHCJS__)
import GHC.Event
# endif
#endif

#if __GLASGOW_HASKELL__ >= 701 && __GLASGOW_HASKELL__ < 710
import GHC.Generics as Generics
# endif

#if MIN_VERSION_base(4,5,0) && __GLASGOW_HASKELL__ < 710
import GHC.Stack
import GHC.Stats
#endif

#if MIN_VERSION_base(4,6,0) && __GLASGOW_HASKELL__ < 710
import Data.Bits
import Data.Ord
import GHC.ForeignPtr
import GHC.GHCi
import GHC.TypeLits
import System.Posix.Internals
#endif

#if !(MIN_VERSION_base(4,6,0))
import Control.Monad (ap, mplus, mzero)
#endif

#if MIN_VERSION_base(4,7,0) && __GLASGOW_HASKELL__ < 710
import Control.Concurrent.QSem
import Data.Proxy
import Text.Read.Lex (Number)
# endif

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
import Control.Arrow
import Control.Category hiding ((.))
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Zip
import Data.Ix
import Data.Type.Coercion
import Data.Type.Equality
import Data.Typeable.Internal
import GHC.Exts as Exts
import GHC.IO.BufferedIO
import GHC.IO.Device (IODevice, RawIO)
import GHC.IO.Handle
import GHC.IO.Handle.Types hiding (BufferList, HandleType)
import GHC.IP
import Text.Printf

# if defined(mingw32_HOST_OS)
import GHC.ConsoleHandler as Console
#  if !defined(__GHCJS__)
import GHC.Conc.Windows
#  endif
# endif
#endif

#if !(MIN_VERSION_base(4,8,0))
import Data.Complex (Complex(..))
import Data.Version
import Foreign.Ptr (castPtr)
import GHC.Real (Ratio(..), (%))
#endif

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Control.Exception as Exception
import Control.Monad.ST.Lazy as Lazy
import Data.Char
import Data.Data as Data
import Data.Foldable
import Data.Monoid as Monoid
import Data.Traversable
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Pool
import Foreign.Storable
import GHC.Base
import GHC.Conc
import GHC.Desugar
import GHC.IO.Buffer
import GHC.IO.Device (IODeviceType(..))
import GHC.IO.Encoding
import GHC.IO.Exception as Exception
import GHC.IO.Handle.Types (BufferList, HandleType)
import GHC.ST
import System.Console.GetOpt
import System.IO
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read as Read

# if defined(mingw32_HOST_OS)
import GHC.IO.Encoding.CodePage.Table
# endif
#endif

-------------------------------------------------------------------------------

#if MIN_VERSION_base(4,4,0) && !(MIN_VERSION_base(4,7,0))
instance Show Fingerprint where
    show (Fingerprint w1 w2) = hex16 w1 ++ hex16 w2
      where
        -- Formats a 64 bit number as 16 digits hex.
        hex16 :: Word64 -> String
        hex16 i = let hex = showHex i ""
                   in replicate (16 - length hex) '0' ++ hex
#endif

#if !(MIN_VERSION_base(4,4,0))
instance HasResolution a => Read (Fixed a) where
    readsPrec _ = readsFixed

readsFixed :: (HasResolution a) => ReadS (Fixed a)
readsFixed = readsSigned
    where readsSigned ('-' : xs) = [ (negate x, rest)
                                   | (x, rest) <- readsUnsigned xs ]
          readsSigned xs = readsUnsigned xs
          readsUnsigned xs = case span isDigit xs of
                             ([], _) -> []
                             (is, xs') ->
                                 let i = fromInteger (read is)
                                 in case xs' of
                                    '.' : xs'' ->
                                        case span isDigit xs'' of
                                        ([], _) -> []
                                        (js, xs''') ->
                                            let j = fromInteger (read js)
                                                l = genericLength js :: Integer
                                            in [(i + (j / (10 ^ l)), xs''')]
                                    _ -> [(i, xs')]

deriving instance Typeable1 SampleVar

instance Applicative (Strict.ST s) where
    pure  = return
    (<*>) = ap

instance Applicative (Lazy.ST s) where
    pure  = return
    (<*>) = ap
#endif

-- These instances are only valid if Bits isn't a subclass of Num (as Bool is
-- not a Num instance), which is only true as of base-4.6.0.0 and later.
#if MIN_VERSION_base(4,6,0) && !(MIN_VERSION_base(4,7,0))
instance Bits Bool where
    (.&.) = (&&)

    (.|.) = (||)

    xor = (/=)

    complement = not

    shift x 0 = x
    shift _ _ = False

    rotate x _ = x

    bit 0 = True
    bit _ = False

    testBit x 0 = x
    testBit _ _ = False

    bitSize _ = 1

    isSigned _ = False

    popCount False = 0
    popCount True  = 1

deriving instance Read a => Read (Down a)
deriving instance Show a => Show (Down a)
#endif

#if !(MIN_VERSION_base(4,6,0))
instance Applicative ReadP where
    pure  = return
    (<*>) = ap

instance Alternative ReadP where
    empty = mzero
    (<|>) = mplus

instance Applicative ReadPrec where
    pure  = return
    (<*>) = ap

instance Alternative ReadPrec where
    empty = mzero
    (<|>) = mplus
#endif

#if !(MIN_VERSION_base(4,7,0))
deriving instance Foldable (Const m)
deriving instance Foldable (Either a)
deriving instance Traversable (Const m)
deriving instance Traversable (Either a)

instance Foldable ((,) a) where
    foldMap f (_, y) = f y
    
    foldr f z (_, y) = f y z

instance Traversable ((,) a) where
    traverse f (x, y) = (,) x <$> f y

deriving instance Monoid a => Monoid (Const a b)
deriving instance Eq ErrorCall
deriving instance Ord ErrorCall
deriving instance Num a => Num (Sum a)
deriving instance Num a => Num (Product a)
deriving instance Data Version
-- GHC Trac #8218
deriving instance Monad m => Monad (WrappedMonad m)
deriving instance Eq a => Eq (ZipList a)
deriving instance Ord a => Ord (ZipList a)
deriving instance Read a => Read (ZipList a)
deriving instance Show a => Show (ZipList a)
deriving instance Functor ArgOrder
deriving instance Functor OptDescr
deriving instance Functor ArgDescr
#endif

#if __GLASGOW_HASKELL__ >= 701 && !(MIN_VERSION_base(4,7,0))
-----
-- Although DeriveGeneric has been around since GHC 7.2, various bugs cause
-- standalone Generic instances to fail to compile unless a fairly recent
-- version of GHC is used. For this reason, we manually implement all Generic
-- and Generic1 instances to maintain GHC 7.2 and 7.4 support.
-----

instance Generic All where
    type Rep All = D1 D1All (C1 C1_0All (S1 S1_0_0All (Rec0 Bool)))
    from (All a) = M1 (M1 (M1 (K1 a)))
    to (M1 (M1 (M1 (K1 a)))) = All a

data D1All
data C1_0All
data S1_0_0All

instance Datatype D1All where
    datatypeName _ = "All"
    moduleName   _ = "Data.Monoid"

instance Constructor C1_0All where
    conName     _ = "All"
    conIsRecord _ = True

instance Selector S1_0_0All where
    selName _ = "getAll"

-----

instance Generic Any where
    type Rep Any = D1 D1Any (C1 C1_0Any (S1 S1_0_0Any (Rec0 Bool)))
    from (Any a) = M1 (M1 (M1 (K1 a)))
    to (M1 (M1 (M1 (K1 a)))) = Any a

data D1Any
data C1_0Any
data S1_0_0Any

instance Datatype D1Any where
    datatypeName _ = "Any"
    moduleName   _ = "Data.Monoid"

instance Constructor C1_0Any where
    conName     _ = "Any"
    conIsRecord _ = True

instance Selector S1_0_0Any where
    selName _ = "getAny"

-----

instance Generic Arity where
    type Rep Arity = D1 D1Arity (C1 C1_0Arity U1
                             :+: C1 C1_1Arity (S1 NoSelector (Rec0 Int)))

    from NoArity   = M1 (L1 (M1 U1))
    from (Arity a) = M1 (R1 (M1 (M1 (K1 a))))

    to (M1 (L1 (M1 U1)))          = NoArity
    to (M1 (R1 (M1 (M1 (K1 a))))) = Arity a

data D1Arity
data C1_0Arity
data C1_1Arity

instance Datatype D1Arity where
    datatypeName _ = "Arity"
    moduleName   _ = "GHC.Generics"

instance Constructor C1_0Arity where
    conName _ = "NoArity"

instance Constructor C1_1Arity where
    conName _ = "Arity"

-----

instance Generic Associativity where
    type Rep Associativity = D1 D1Associativity (C1 C1_0Associativity U1
                                            :+: (C1 C1_1Associativity U1
                                            :+:  C1 C1_2Associativity U1))

    from LeftAssociative  = M1 (L1 (M1 U1))
    from RightAssociative = M1 (R1 (L1 (M1 U1)))
    from NotAssociative   = M1 (R1 (R1 (M1 U1)))

    to (M1 (L1 (M1 U1)))      = LeftAssociative
    to (M1 (R1 (L1 (M1 U1)))) = RightAssociative
    to (M1 (R1 (R1 (M1 U1)))) = NotAssociative

data D1Associativity
data C1_0Associativity
data C1_1Associativity
data C1_2Associativity

instance Datatype D1Associativity where
    datatypeName _ = "Associativity"
    moduleName   _ = "GHC.Generics"

instance Constructor C1_0Associativity where
    conName _ = "LeftAssociative"

instance Constructor C1_1Associativity where
    conName _ = "RightAssociative"

instance Constructor C1_2Associativity where
    conName _ = "NotAssociative"

-----

instance Generic (Const a b) where
    type Rep (Const a b) = D1 D1Const (C1 C1_0Const (S1 S1_0_0Const (Rec0 a)))
    from (Const a) = M1 (M1 (M1 (K1 a)))
    to (M1 (M1 (M1 (K1 a)))) = Const a

instance Generic1 (Const a) where
    type Rep1 (Const a) = D1 D1Const (C1 C1_0Const (S1 S1_0_0Const (Rec0 a)))
    from1 (Const a) = M1 (M1 (M1 (K1 a)))
    to1 (M1 (M1 (M1 (K1 a)))) = Const a

data D1Const
data C1_0Const
data S1_0_0Const

instance Datatype D1Const where
    datatypeName _ = "Const"
    moduleName   _ = "Control.Applicative"

instance Constructor C1_0Const where
    conName     _ = "Const"
    conIsRecord _ = True

instance Selector S1_0_0Const where
    selName _ = "getConst"

-----

instance Generic (Dual a) where
    type Rep (Dual a) = D1 D1Dual (C1 C1_0Dual (S1 S1_0_0Dual (Rec0 a)))
    from (Dual d) = M1 (M1 (M1 (K1 d)))
    to (M1 (M1 (M1 (K1 d)))) = Dual d

instance Generic1 Dual where
    type Rep1 Dual = D1 D1Dual (C1 C1_0Dual (S1 S1_0_0Dual Par1))
    from1 (Dual d) = M1 (M1 (M1 (Par1 d)))
    to1 (M1 (M1 (M1 (Par1 d)))) = Dual d

data D1Dual
data C1_0Dual
data S1_0_0Dual

instance Datatype D1Dual where
    datatypeName _ = "Dual"
    moduleName   _ = "Data.Monoid"

instance Constructor C1_0Dual where
    conName     _ = "Dual"
    conIsRecord _ = True

instance Selector S1_0_0Dual where
    selName _ = "getDual"

-----

instance Generic (Endo a) where
    type Rep (Endo a) = D1 D1Endo (C1 C1_0Endo (S1 S1_0_0Endo (Rec0 (a -> a))))
    from (Endo e) = M1 (M1 (M1 (K1 e)))
    to (M1 (M1 (M1 (K1 e)))) = Endo e

data D1Endo
data C1_0Endo
data S1_0_0Endo

instance Datatype D1Endo where
    datatypeName _ = "Endo"
    moduleName   _ = "Data.Monoid"

instance Constructor C1_0Endo where
    conName     _ = "Endo"
    conIsRecord _ = True

instance Selector S1_0_0Endo where
    selName _ = "appEndo"

-----

instance Generic (First a) where
    type Rep (First a) = D1 D1First (C1 C1_0First (S1 S1_0_0First (Rec0 (Maybe a))))
    from (First f) = M1 (M1 (M1 (K1 f)))
    to (M1 (M1 (M1 (K1 f)))) = First f

instance Generic1 First where
    type Rep1 First = D1 D1First (C1 C1_0First (S1 S1_0_0First (Rec1 Maybe)))
    from1 (First f) = M1 (M1 (M1 (Rec1 f)))
    to1 (M1 (M1 (M1 (Rec1 f)))) = First f

data D1First
data C1_0First
data S1_0_0First

instance Datatype D1First where
    datatypeName _ = "First"
    moduleName   _ = "Data.Monoid"

instance Constructor C1_0First where
    conName     _ = "First"
    conIsRecord _ = True

instance Selector S1_0_0First where
    selName _ = "getFirst"

-----

instance Generic Generics.Fixity where
    type Rep Generics.Fixity = D1 D1Fixity (C1 C1_0Fixity U1
                                        :+: C1 C1_1Fixity (S1 NoSelector (Rec0 Associativity)
                                                       :*: S1 NoSelector (Rec0 Int)))

    from Generics.Prefix      = M1 (L1 (M1 U1))
    from (Generics.Infix a i) = M1 (R1 (M1 (M1 (K1 a) :*: M1 (K1 i))))

    to (M1 (L1 (M1 U1)))                        = Generics.Prefix
    to (M1 (R1 (M1 (M1 (K1 a) :*: M1 (K1 i))))) = Generics.Infix a i

data D1Fixity
data C1_0Fixity
data C1_1Fixity

instance Datatype D1Fixity where
    datatypeName _ = "Fixity"
    moduleName   _ = "GHC.Generics"

instance Constructor C1_0Fixity where
    conName _ = "Prefix"

instance Constructor C1_1Fixity where
    conName _ = "Infix"

-----

instance Generic (Last a) where
    type Rep (Last a) = D1 D1Last (C1 C1_0Last (S1 S1_0_0Last (Rec0 (Maybe a))))
    from (Last l) = M1 (M1 (M1 (K1 l)))
    to (M1 (M1 (M1 (K1 l)))) = Last l

instance Generic1 Last where
    type Rep1 Last = D1 D1Last (C1 C1_0Last (S1 S1_0_0Last (Rec1 Maybe)))
    from1 (Last l) = M1 (M1 (M1 (Rec1 l)))
    to1 (M1 (M1 (M1 (Rec1 l)))) = Last l

data D1Last
data C1_0Last
data S1_0_0Last

instance Datatype D1Last where
    datatypeName _ = "Last"
    moduleName   _ = "Data.Monoid"

instance Constructor C1_0Last where
    conName     _ = "Last"
    conIsRecord _ = True

instance Selector S1_0_0Last where
    selName _ = "getLast"

-----

instance Generic (Product a) where
    type Rep (Product a) = D1 D1Product (C1 C1_0Product (S1 S1_0_0Product (Rec0 a)))
    from (Product p) = M1 (M1 (M1 (K1 p)))
    to (M1 (M1 (M1 (K1 p)))) = Product p

instance Generic1 Product where
    type Rep1 Product = D1 D1Product (C1 C1_0Product (S1 S1_0_0Product Par1))
    from1 (Product p) = M1 (M1 (M1 (Par1 p)))
    to1 (M1 (M1 (M1 (Par1 p)))) = Product p

data D1Product
data C1_0Product
data S1_0_0Product

instance Datatype D1Product where
    datatypeName _ = "Product"
    moduleName   _ = "Data.Monoid"

instance Constructor C1_0Product where
    conName     _ = "Product"
    conIsRecord _ = True

instance Selector S1_0_0Product where
    selName _ = "getProduct"

-----

instance Generic (Sum a) where
    type Rep (Sum a) = D1 D1Sum (C1 C1_0Sum (S1 S1_0_0Sum (Rec0 a)))
    from (Sum s) = M1 (M1 (M1 (K1 s)))
    to (M1 (M1 (M1 (K1 s)))) = Sum s

instance Generic1 Sum where
    type Rep1 Sum = D1 D1Sum (C1 C1_0Sum (S1 S1_0_0Sum Par1))
    from1 (Sum s) = M1 (M1 (M1 (Par1 s)))
    to1 (M1 (M1 (M1 (Par1 s)))) = Sum s

data D1Sum
data C1_0Sum
data S1_0_0Sum

instance Datatype D1Sum where
    datatypeName _ = "Sum"
    moduleName   _ = "Data.Monoid"

instance Constructor C1_0Sum where
    conName     _ = "Sum"
    conIsRecord _ = True

instance Selector S1_0_0Sum where
    selName _ = "getSum"

-----

instance Generic (WrappedArrow a b c) where
    type Rep (WrappedArrow a b c)
      = D1 D1WrappedArrow (C1 C1_0WrappedArrow (S1 S1_0_0WrappedArrow (Rec0 (a b c))))
    from (WrapArrow a) = M1 (M1 (M1 (K1 a)))
    to (M1 (M1 (M1 (K1 a)))) = WrapArrow a

instance Generic1 (WrappedArrow a b) where
    type Rep1 (WrappedArrow a b)
      = D1 D1WrappedArrow (C1 C1_0WrappedArrow (S1 S1_0_0WrappedArrow (Rec1 (a b))))
    from1 (WrapArrow a) = M1 (M1 (M1 (Rec1 a)))
    to1 (M1 (M1 (M1 (Rec1 a)))) = WrapArrow a

data D1WrappedArrow
data C1_0WrappedArrow
data S1_0_0WrappedArrow

instance Datatype D1WrappedArrow where
  datatypeName _ = "WrappedArrow"
  moduleName   _ = "Control.Applicative"

instance Constructor C1_0WrappedArrow where
  conName     _ = "WrapArrow"
  conIsRecord _ = True

instance Selector S1_0_0WrappedArrow where
  selName _ = "unwrapArrow"

-----

instance Generic (WrappedMonad m a) where
    type Rep (WrappedMonad m a)
      = D1 D1WrappedMonad (C1 C1_0WrappedMonad (S1 S1_0_0WrappedMonad (Rec0 (m a))))
    from (WrapMonad m) = M1 (M1 (M1 (K1 m)))
    to (M1 (M1 (M1 (K1 m)))) = WrapMonad m

instance Generic1 (WrappedMonad m) where
    type Rep1 (WrappedMonad m)
      = D1 D1WrappedMonad (C1 C1_0WrappedMonad (S1 S1_0_0WrappedMonad (Rec1 m)))
    from1 (WrapMonad m) = M1 (M1 (M1 (Rec1 m)))
    to1 (M1 (M1 (M1 (Rec1 m)))) = WrapMonad m

data D1WrappedMonad
data C1_0WrappedMonad
data S1_0_0WrappedMonad

instance Datatype D1WrappedMonad where
    datatypeName _ = "WrappedMonad"
    moduleName   _ = "Control.Applicative"

instance Constructor C1_0WrappedMonad where
    conName     _ = "WrapMonad"
    conIsRecord _ = True

instance Selector S1_0_0WrappedMonad where
    selName _ = "unwrapMonad"

-----

instance Generic (ZipList a) where
    type Rep (ZipList a) = D1 D1ZipList (C1 C1_0ZipList (S1 S1_0_0ZipList (Rec0 [a])))
    from (ZipList z) = M1 (M1 (M1 (K1 z)))
    to (M1 (M1 (M1 (K1 z)))) = ZipList z

instance Generic1 ZipList where
    type Rep1 ZipList = D1 D1ZipList (C1 C1_0ZipList (S1 S1_0_0ZipList (Rec1 [])))
    from1 (ZipList z) = M1 (M1 (M1 (Rec1 z)))
    to1 (M1 (M1 (M1 (Rec1 z)))) = ZipList z

data D1ZipList
data C1_0ZipList
data S1_0_0ZipList

instance Datatype D1ZipList where
    datatypeName _ = "ZipList"
    moduleName   _ = "Control.Applicative"

instance Constructor C1_0ZipList where
    conName     _ = "ZipList"
    conIsRecord _ = True

instance Selector S1_0_0ZipList where
    selName _ = "getZipList"

-----

instance Generic (U1 p) where
    type Rep (U1 p) = D1 D1U1 (C1 C1_0U1 U1)
    from U1 = M1 (M1 U1)
    to (M1 (M1 U1)) = U1

data D1U1
data C1_0U1

instance Datatype D1U1 where
    datatypeName _ = "U1"
    moduleName   _ = "GHC.Generics"

instance Constructor C1_0U1 where
    conName _ = "U1"

-----

instance Generic (Par1 p) where
    type Rep (Par1 p) = D1 D1Par1 (C1 C1_0Par1 (S1 S1_0_0Par1 (Rec0 p)))
    from (Par1 p) = M1 (M1 (M1 (K1 p)))
    to (M1 (M1 (M1 (K1 p)))) = Par1 p

data D1Par1
data C1_0Par1
data S1_0_0Par1

instance Datatype D1Par1 where
    datatypeName _ = "Par1"
    moduleName   _ = "GHC.Generics"

instance Constructor C1_0Par1 where
    conName     _ = "Par1"
    conIsRecord _ = True

instance Selector S1_0_0Par1 where
    selName _ = "unPar1"

-----

instance Generic (Rec1 f p) where
    type Rep (Rec1 f p)
      = D1 D1Rec1 (C1 C1_0Rec1 (S1 S1_0_0Rec1 (Rec0 (f p))))
    from (Rec1 r) = M1 (M1 (M1 (K1 r)))
    to (M1 (M1 (M1 (K1 r)))) = Rec1 r

data D1Rec1
data C1_0Rec1
data S1_0_0Rec1

instance Datatype D1Rec1 where
    datatypeName _ = "Rec1"
    moduleName   _ = "GHC.Generics"

instance Constructor C1_0Rec1 where
    conName     _ = "Rec1"
    conIsRecord _ = True

instance Selector S1_0_0Rec1 where
    selName _ = "unRec1"

-----

instance Generic (K1 i c p) where
    type Rep (K1 i c p) = D1 D1K1 (C1 C1_0K1 (S1 S1_0_0K1 (Rec0 c)))
    from (K1 c) = M1 (M1 (M1 (K1 c)))
    to (M1 (M1 (M1 (K1 c)))) = K1 c

data D1K1
data C1_0K1
data S1_0_0K1

instance Datatype D1K1 where
    datatypeName _ = "K1"
    moduleName   _ = "GHC.Generics"

instance Constructor C1_0K1 where
    conName     _ = "K1"
    conIsRecord _ = True

instance Selector S1_0_0K1 where
    selName _ = "unK1"

-----

instance Generic (M1 i c f p) where
    type Rep (M1 i c f p) = D1 D1M1 (C1 C1_0M1 (S1 S1_0_0M1 (Rec0 (f p))))
    from (M1 m) = M1 (M1 (M1 (K1 m)))
    to (M1 (M1 (M1 (K1 m)))) = M1 m

data D1M1
data C1_0M1
data S1_0_0M1

instance Datatype D1M1 where
    datatypeName _ = "M1"
    moduleName   _ = "GHC.Generics"

instance Constructor C1_0M1 where
    conName     _ = "M1"
    conIsRecord _ = True

instance Selector S1_0_0M1 where
    selName _ = "unM1"

-----

instance Generic ((f :+: g) p) where
    type Rep ((f :+: g) p) = D1 D1ConSum (C1 C1_0ConSum (S1 NoSelector (Rec0 (f p)))
                                      :+: C1 C1_1ConSum (S1 NoSelector (Rec0 (g p))))

    from (L1 l) = M1 (L1 (M1 (M1 (K1 l))))
    from (R1 r) = M1 (R1 (M1 (M1 (K1 r))))

    to (M1 (L1 (M1 (M1 (K1 l))))) = L1 l
    to (M1 (R1 (M1 (M1 (K1 r))))) = R1 r

data D1ConSum
data C1_0ConSum
data C1_1ConSum

instance Datatype D1ConSum where
    datatypeName _ = ":+:"
    moduleName   _ = "GHC.Generics"

instance Constructor C1_0ConSum where
    conName _ = "L1"

instance Constructor C1_1ConSum where
    conName _ = "R1"

-----

instance Generic ((f :*: g) p) where
    type Rep ((f :*: g) p) =
        D1 D1ConProduct (C1 C1_ConProduct (S1 NoSelector (Rec0 (f p))
                                       :*: S1 NoSelector (Rec0 (g p))))
    from (f :*: g) = M1 (M1 (M1 (K1 f) :*: M1 (K1 g)))
    to (M1 (M1 (M1 (K1 f) :*: M1 (K1 g)))) = f :*: g

data D1ConProduct
data C1_ConProduct

instance Datatype D1ConProduct where
    datatypeName _ = ":*:"
    moduleName   _ = "GHC.Generics"

instance Constructor C1_ConProduct where
    conName   _ = ":*:"
    conFixity _ = Generics.Infix RightAssociative 6

-----

instance Generic ((f :.: g) p) where
    type Rep ((f :.: g) p)
      = D1 D1ConCompose (C1 C1_0ConCompose (S1 S1_0_0ConCompose (Rec0 (f (g p)))))
    from (Comp1 c) = M1 (M1 (M1 (K1 c)))
    to (M1 (M1 (M1 (K1 c)))) = Comp1 c

data D1ConCompose
data C1_0ConCompose
data S1_0_0ConCompose

instance Datatype D1ConCompose where
    datatypeName _ = ":.:"
    moduleName   _ = "GHC.Generics"

instance Constructor C1_0ConCompose where
  conName     _ = "Comp1"
  conIsRecord _ = True

instance Selector S1_0_0ConCompose where
  selName _ = "unComp1"

-----

# if !(MIN_VERSION_base(4,6,0))
instance Generic1 [] where
    type Rep1 [] = D1 D1List (C1 C1_0List U1 :+:
                              C1 C1_1List (S1 NoSelector Par1
                                       :*: S1 NoSelector (Rec1 [])))

    from1 []    = M1 (L1 (M1 U1))
    from1 (h:t) = M1 (R1 (M1 (M1 (Par1 h) :*: M1 (Rec1 t))))

    to1 (M1 (L1 (M1 U1)))                            = []
    to1 (M1 (R1 (M1 (M1 (Par1 h) :*: M1 (Rec1 t))))) = h : t

data D1List
data C1_0List
data C1_1List

instance Datatype D1List where
    datatypeName _ = "[]"
    moduleName   _ = "GHC.Types"

instance Constructor C1_0List  where
    conName _ = "[]"

instance Constructor C1_1List where
    conName   _ = ":"
    conFixity _ = Generics.Infix RightAssociative 5

-----

instance Generic1 (Either a) where
    type Rep1 (Either a) = D1 D1Either (C1 C1_0Either (S1 NoSelector (Rec0 a))
                                    :+: C1 C1_1Either (S1 NoSelector Par1))

    from1 (Left l) = M1 (L1 (M1 (M1 (K1 l))))
    from1 (Right r) = M1 (R1 (M1 (M1 (Par1 r))))

    to1 (M1 (L1 (M1 (M1 (K1 l))))) = Left l
    to1 (M1 (R1 (M1 (M1 (Par1 r))))) = Right r

data D1Either
data C1_0Either
data C1_1Either

instance Datatype D1Either where
    datatypeName _ = "Either"
    moduleName   _ = "Data.Either"

instance Constructor C1_0Either where
    conName _ = "Left"

instance Constructor C1_1Either where
    conName _ = "Right"

-----

instance Generic1 Maybe where
    type Rep1 Maybe = D1 D1Maybe (C1 C1_0Maybe U1
                              :+: C1 C1_1Maybe (S1 NoSelector Par1))

    from1 Nothing  = M1 (L1 (M1 U1))
    from1 (Just j) = M1 (R1 (M1 (M1 (Par1 j))))

    to1 (M1 (L1 (M1 U1)))            = Nothing
    to1 (M1 (R1 (M1 (M1 (Par1 j))))) = Just j

data D1Maybe
data C1_0Maybe
data C1_1Maybe

instance Datatype D1Maybe where
    datatypeName _ = "Maybe"
    -- As of base-4.7.0.0, Maybe is actually located in GHC.Base.
    -- We don't need to worry about this for the versions of base
    -- that this instance is defined for, however.
    moduleName   _ = "Data.Maybe"

instance Constructor C1_0Maybe where
    conName _ = "Nothing"

instance Constructor C1_1Maybe where
    conName _ = "Just"

-----

instance Generic1 ((,) a) where
    type Rep1 ((,) a) = D1 D1Tuple2 (C1 C1_0Tuple2 (S1 NoSelector (Rec0 a)
                                                :*: S1 NoSelector Par1))
    from1 (a, b) = M1 (M1 (M1 (K1 a) :*: M1 (Par1 b)))
    to1 (M1 (M1 (M1 (K1 a) :*: M1 (Par1 b)))) = (a, b)

data D1Tuple2
data C1_0Tuple2

instance Datatype D1Tuple2 where
    datatypeName _ = "(,)"
    moduleName   _ = "GHC.Tuple"

instance Constructor C1_0Tuple2 where
    conName _ = "(,)"

-----

instance Generic1 ((,,) a b) where
    type Rep1 ((,,) a b) = D1 D1Tuple3 (C1 C1_0Tuple3 (S1 NoSelector (Rec0 a)
                                                  :*: (S1 NoSelector (Rec0 b)
                                                  :*:  S1 NoSelector Par1)))
    from1 (a, b, c) = M1 (M1 (M1 (K1 a) :*: (M1 (K1 b) :*: M1 (Par1 c))))
    to1 (M1 (M1 (M1 (K1 a) :*: (M1 (K1 b) :*: M1 (Par1 c))))) = (a, b, c)

data D1Tuple3
data C1_0Tuple3

instance Datatype D1Tuple3 where
    datatypeName _ = "(,,)"
    moduleName   _ = "GHC.Tuple"

instance Constructor C1_0Tuple3 where
    conName _ = "(,,)"

-----

instance Generic1 ((,,,) a b c) where
    type Rep1 ((,,,) a b c) = D1 D1Tuple4 (C1 C1_0Tuple4 ((S1 NoSelector (Rec0 a)
                                                      :*:  S1 NoSelector (Rec0 b))
                                                      :*: (S1 NoSelector (Rec0 c)
                                                      :*:  S1 NoSelector Par1)))

    from1 (a, b, c, d) = M1 (M1 ((M1 (K1 a) :*: M1 (K1 b))
                             :*: (M1 (K1 c) :*: M1 (Par1 d))))

    to1 (M1 (M1 ((M1 (K1 a) :*: M1 (K1 b))
             :*: (M1 (K1 c) :*: M1 (Par1 d)))))
      = (a, b, c, d)

data D1Tuple4
data C1_0Tuple4

instance Datatype D1Tuple4 where
    datatypeName _ = "(,,,)"
    moduleName _ = "GHC.Tuple"

instance Constructor C1_0Tuple4 where
    conName _ = "(,,,)"

-----

instance Generic1 ((,,,,) a b c d) where
    type Rep1 ((,,,,) a b c d) = D1 D1Tuple5 (C1 C1_0Tuple5 ((S1 NoSelector (Rec0 a)
                                                         :*:  S1 NoSelector (Rec0 b))
                                                         :*: (S1 NoSelector (Rec0 c)
                                                         :*: (S1 NoSelector (Rec0 d)
                                                         :*:  S1 NoSelector Par1))))

    from1 (a, b, c, d, e) = M1 (M1 ((M1 (K1 a) :*: M1 (K1 b))
                                :*: (M1 (K1 c) :*: (M1 (K1 d) :*: M1 (Par1 e)))))

    to1 (M1 (M1 ((M1 (K1 a) :*: M1 (K1 b))
                                :*: (M1 (K1 c) :*: (M1 (K1 d) :*: M1 (Par1 e))))))
      = (a, b, c, d, e)

data D1Tuple5
data C1_0Tuple5

instance Datatype D1Tuple5 where
    datatypeName _ = "(,,,,)"
    moduleName   _ = "GHC.Tuple"

instance Constructor C1_0Tuple5 where
    conName _ = "(,,,,)"

-----

instance Generic1 ((,,,,,) a b c d e) where
    type Rep1 ((,,,,,) a b c d e)
      = D1 D1Tuple6 (C1 C1_0Tuple6 ((S1 NoSelector (Rec0 a)
                                :*: (S1 NoSelector (Rec0 b)
                                :*:  S1 NoSelector (Rec0 c)))
                                :*: (S1 NoSelector (Rec0 d)
                                :*: (S1 NoSelector (Rec0 e)
                                :*:  S1 NoSelector Par1))))

    from1 (a, b, c, d, e, f) = M1 (M1 ((M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c)))
                                   :*: (M1 (K1 d) :*: (M1 (K1 e) :*: M1 (Par1 f)))))

    to1 (M1 (M1 ((M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c)))
             :*: (M1 (K1 d) :*: (M1 (K1 e) :*: M1 (Par1 f))))))
      = (a, b, c, d, e, f)

data D1Tuple6
data C1_0Tuple6

instance Datatype D1Tuple6 where
    datatypeName _ = "(,,,,,)"
    moduleName   _ = "GHC.Tuple"

instance Constructor C1_0Tuple6 where
    conName _ = "(,,,,,)"

-----

instance Generic1 ((,,,,,,) a b c d e f) where
    type Rep1 ((,,,,,,) a b c d e f)
      = D1 D1Tuple7 (C1 C1_0Tuple7 ((S1 NoSelector (Rec0 a)
                               :*:  (S1 NoSelector (Rec0 b)
                               :*:   S1 NoSelector (Rec0 c)))
                               :*: ((S1 NoSelector (Rec0 d)
                               :*:   S1 NoSelector (Rec0 e))
                               :*:  (S1 NoSelector (Rec0 f)
                               :*:   S1 NoSelector Par1))))

    from1 (a, b, c, d, e, f, g) = M1 (M1 ((M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c)))
                      :*: ((M1 (K1 d) :*: M1 (K1 e)) :*: (M1 (K1 f) :*: M1 (Par1 g)))))

    to1 (M1 (M1 ((M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c)))
            :*: ((M1 (K1 d) :*: M1 (K1 e)) :*: (M1 (K1 f) :*: M1 (Par1 g))))))
      = (a, b, c, d, e, f, g)

data D1Tuple7
data C1_0Tuple7

instance Datatype D1Tuple7 where
    datatypeName _ = "(,,,,,,)"
    moduleName   _ = "GHC.Tuple"

instance Constructor C1_0Tuple7 where
    conName _ = "(,,,,,,)"
# endif

-----

deriving instance Eq (U1 p)
deriving instance Ord (U1 p)
deriving instance Read (U1 p)
deriving instance Show (U1 p)

deriving instance Eq p => Eq (Par1 p)
deriving instance Ord p => Ord (Par1 p)
deriving instance Read p => Read (Par1 p)
deriving instance Show p => Show (Par1 p)

deriving instance Eq (f p) => Eq (Rec1 f p)
deriving instance Ord (f p) => Ord (Rec1 f p)
deriving instance Read (f p) => Read (Rec1 f p)
deriving instance Show (f p) => Show (Rec1 f p)

deriving instance Eq c => Eq (K1 i c p)
deriving instance Ord c => Ord (K1 i c p)
deriving instance Read c => Read (K1 i c p)
deriving instance Show c => Show (K1 i c p)

deriving instance Eq (f p) => Eq (M1 i c f p)
deriving instance Ord (f p) => Ord (M1 i c f p)
deriving instance Read (f p) => Read (M1 i c f p)
deriving instance Show (f p) => Show (M1 i c f p)

deriving instance (Eq (f p), Eq (g p)) => Eq ((f :+: g) p)
deriving instance (Ord (f p), Ord (g p)) => Ord ((f :+: g) p)
deriving instance (Read (f p), Read (g p)) => Read ((f :+: g) p)
deriving instance (Show (f p), Show (g p)) => Show ((f :+: g) p)

deriving instance (Eq (f p), Eq (g p)) => Eq ((f :*: g) p)
deriving instance (Ord (f p), Ord (g p)) => Ord ((f :*: g) p)
-- Due to a GHC bug (https://ghc.haskell.org/trac/ghc/ticket/9830), the derived
-- Read and Show instances for infix data constructors will use the wrong
-- precedence (prior to GHC 7.10).
-- We'll manually derive Read :*: and Show :*: instances to avoid this.
instance (Read (f p), Read (g p)) => Read ((f :*: g) p) where
    readPrec = parens . ReadPrec.prec 6 $ do
        fp <- ReadPrec.step readPrec
        Symbol ":*:" <- lexP
        gp <- ReadPrec.step readPrec
        return $ fp :*: gp
    readListPrec = readListPrecDefault
instance (Show (f p), Show (g p)) => Show ((f :*: g) p) where
     showsPrec p (l :*: r) = showParen (p > sixPrec) $
            showsPrec (sixPrec + 1) l
         . showString " :*: "
         . showsPrec (sixPrec + 1) r
       where sixPrec = 6

deriving instance Eq (f (g p)) => Eq ((f :.: g) p)
deriving instance Ord (f (g p)) => Ord ((f :.: g) p)
deriving instance Read (f (g p)) => Read ((f :.: g) p)
deriving instance Show (f (g p)) => Show ((f :.: g) p)
#endif

#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
-- | Construct tag-less 'Version'
--
-- /Since: 4.8.0.0/
makeVersion :: [Int] -> Version
makeVersion b = Version b []

-- | /Since: 4.8.0.0/
instance IsList Version where
  type (Item Version) = Int
  fromList = makeVersion
  toList = versionBranch
#endif

#if !(MIN_VERSION_base(4,8,0))
deriving instance Eq a => Eq (Const a b)
deriving instance Ord a => Ord (Const a b)

instance Read a => Read (Const a b) where
    readsPrec d = readParen (d > 10)
        $ \r -> [(Const x,t) | ("Const", s) <- lex r, (x, t) <- readsPrec 11 s]

instance Show a => Show (Const a b) where
    showsPrec d (Const x) = showParen (d > 10) $
                            showString "Const " . showsPrec 11 x

deriving instance Functor First
deriving instance Applicative First
deriving instance Monad First
deriving instance Functor Last
deriving instance Applicative Last
deriving instance Monad Last

-- In base-4.3 and earlier, pattern matching on a Complex value invokes a
-- RealFloat constraint due to the use of the DatatypeContexts extension.
# if MIN_VERSION_base(4,4,0)
instance Storable a
# else
instance (Storable a, RealFloat a)
# endif
  => Storable (Complex a) where
    sizeOf (a :+ _)    = 2 * sizeOf a
    alignment (a :+ _) = alignment a
    peek p           = do
                        q <- return $ castPtr p
                        r <- peek q
                        i <- peekElemOff q 1
                        return (r :+ i)
    poke p (r :+ i)  = do
                        q <-return $  (castPtr p)
                        poke q r
                        pokeElemOff q 1 i

instance (Storable a, Integral a) => Storable (Ratio a) where
    sizeOf (n :% _)    = 2 * sizeOf n
    alignment (n :% _) = alignment n
    peek p           = do
                        q <- return $ castPtr p
                        r <- peek q
                        i <- peekElemOff q 1
                        return (r % i)
    poke p (r :% i)  = do
                        q <-return $  (castPtr p)
                        poke q r
                        pokeElemOff q 1 i
#endif

#if __GLASGOW_HASKELL__ < 710
deriving instance Typeable  All
deriving instance Typeable  AnnotationWrapper
deriving instance Typeable  Monoid.Any
deriving instance Typeable1 ArgDescr
deriving instance Typeable1 ArgOrder
deriving instance Typeable  BlockReason
deriving instance Typeable1 Buffer
deriving instance Typeable3 BufferCodec
deriving instance Typeable1 BufferList
deriving instance Typeable  BufferMode
deriving instance Typeable  BufferState
deriving instance Typeable  CFile
deriving instance Typeable  CFpos
deriving instance Typeable  CJmpBuf
deriving instance Typeable2 Const
deriving instance Typeable  Constr
deriving instance Typeable  ConstrRep
deriving instance Typeable  DataRep
deriving instance Typeable  DataType
deriving instance Typeable1 Dual
deriving instance Typeable1 Endo
deriving instance Typeable  Errno
deriving instance Typeable1 First
deriving instance Typeable  Data.Fixity
deriving instance Typeable  GeneralCategory
deriving instance Typeable  HandlePosn
deriving instance Typeable1 Exception.Handler
deriving instance Typeable  HandleType
deriving instance Typeable  IODeviceType
deriving instance Typeable  IOErrorType
deriving instance Typeable  IOMode
deriving instance Typeable1 Last
deriving instance Typeable  Lexeme
deriving instance Typeable  Newline
deriving instance Typeable  NewlineMode
deriving instance Typeable  Opaque
deriving instance Typeable1 OptDescr
deriving instance Typeable  Pool
deriving instance Typeable1 Product
deriving instance Typeable1 ReadP
deriving instance Typeable1 ReadPrec
deriving instance Typeable  SeekMode
deriving instance Typeable2 Lazy.ST
deriving instance Typeable2 STret
deriving instance Typeable1 Sum
deriving instance Typeable  TextEncoding
deriving instance Typeable  ThreadStatus
deriving instance Typeable1 ZipList

# if defined(mingw32_HOST_OS)
deriving instance Typeable  CodePageArrays
deriving instance Typeable2 CompactArray
deriving instance Typeable1 ConvArray
deriving instance Typeable  Console.Handler
# endif

# if MIN_VERSION_base(4,3,0)
deriving instance Typeable  MaskingState
# endif

# if MIN_VERSION_base(4,4,0)
deriving instance Typeable  CodingFailureMode
deriving instance Typeable  CodingProgress
deriving instance Typeable  Fingerprint

#  if !defined(mingw32_HOST_OS) && !defined(__GHCJS__)
deriving instance Typeable  Event
deriving instance Typeable  EventManager
deriving instance Typeable  FdKey
deriving instance Typeable  TimeoutKey
#  endif
# endif

# if __GLASGOW_HASKELL__ >= 701
deriving instance Typeable  Arity
deriving instance Typeable  Associativity
deriving instance Typeable  C
deriving instance Typeable  D
deriving instance Typeable  Generics.Fixity
deriving instance Typeable3 K1
deriving instance Typeable  NoSelector
deriving instance Typeable  P
deriving instance Typeable1 Par1
deriving instance Typeable  R
deriving instance Typeable  S
deriving instance Typeable1 U1
deriving instance Typeable1 V1
# endif

# if MIN_VERSION_base(4,5,0)
deriving instance Typeable  CostCentre
deriving instance Typeable  CostCentreStack
deriving instance Typeable  GCStats
# endif

# if MIN_VERSION_base(4,6,0)
deriving instance Typeable  CSigset
deriving instance Typeable1 Down
deriving instance Typeable  ForeignPtrContents
deriving instance Typeable  Nat
deriving instance Typeable1 NoIO
deriving instance Typeable  Symbol
# endif

# if MIN_VERSION_ghc_prim(0,3,1)
deriving instance Typeable  SPEC
# endif

# if MIN_VERSION_base(4,7,0)
deriving instance Typeable FieldFormat
deriving instance Typeable FormatAdjustment
deriving instance Typeable FormatParse
deriving instance Typeable FormatSign
deriving instance Typeable KProxy
deriving instance Typeable Number
deriving instance Typeable SomeNat
deriving instance Typeable SomeSymbol
deriving instance Typeable QSem -- This instance seems to have been removed
                                -- (accidentally?) in base-4.7.0.0
# endif

# if __GLASGOW_HASKELL__ >= 708
-- Data types which have more than seven type arguments
deriving instance Typeable (,,,,,,,)
deriving instance Typeable (,,,,,,,,)
deriving instance Typeable (,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)

-- Data types which require PolyKinds
deriving instance Typeable (:+:)
deriving instance Typeable (:*:)
deriving instance Typeable (:.:)
deriving instance Typeable Exts.Any
deriving instance Typeable ArrowMonad
deriving instance Typeable Kleisli
deriving instance Typeable M1
deriving instance Typeable Rec1
deriving instance Typeable WrappedArrow
deriving instance Typeable WrappedMonad

-- Typeclasses
deriving instance Typeable Arrow
deriving instance Typeable ArrowApply
deriving instance Typeable ArrowChoice
deriving instance Typeable ArrowLoop
deriving instance Typeable ArrowZero
deriving instance Typeable Bits
deriving instance Typeable Bounded
deriving instance Typeable BufferedIO
deriving instance Typeable Category
deriving instance Typeable Coercible
deriving instance Typeable Constructor
deriving instance Typeable Data
deriving instance Typeable Datatype
deriving instance Typeable Enum
deriving instance Typeable Exception
deriving instance Typeable Eq
deriving instance Typeable FiniteBits
deriving instance Typeable Floating
deriving instance Typeable Foldable
deriving instance Typeable Fractional
deriving instance Typeable Functor
deriving instance Typeable Generic
deriving instance Typeable Generic1
deriving instance Typeable GHCiSandboxIO
deriving instance Typeable HasResolution
deriving instance Typeable HPrintfType
deriving instance Typeable Integral
deriving instance Typeable IODevice
deriving instance Typeable IP
deriving instance Typeable IsChar
deriving instance Typeable IsList
deriving instance Typeable IsString
deriving instance Typeable Ix
deriving instance Typeable KnownNat
deriving instance Typeable KnownSymbol
deriving instance Typeable Monad
deriving instance Typeable MonadFix
deriving instance Typeable MonadPlus
deriving instance Typeable MonadZip
deriving instance Typeable Num
deriving instance Typeable Ord
deriving instance Typeable PrintfArg
deriving instance Typeable PrintfType
deriving instance Typeable RawIO
deriving instance Typeable Read
deriving instance Typeable Real
deriving instance Typeable RealFloat
deriving instance Typeable RealFrac
deriving instance Typeable Selector
deriving instance Typeable Show
deriving instance Typeable Storable
deriving instance Typeable TestCoercion
deriving instance Typeable TestEquality
deriving instance Typeable Traversable
deriving instance Typeable Typeable

-- Constraints
deriving instance Typeable (~)
deriving instance Typeable Constraint

-- Promoted data constructors
deriving instance Typeable '()
deriving instance Typeable '(,)
deriving instance Typeable '(,,)
deriving instance Typeable '(,,,)
deriving instance Typeable '(,,,,)
deriving instance Typeable '(,,,,,)
deriving instance Typeable '(,,,,,,)
deriving instance Typeable '(,,,,,,,)
deriving instance Typeable '(,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
deriving instance Typeable '[]
deriving instance Typeable '(:)
deriving instance Typeable '(:%)
deriving instance Typeable '(:+)
deriving instance Typeable 'AbsoluteSeek
deriving instance Typeable 'All
deriving instance Typeable 'AlreadyExists
deriving instance Typeable 'Any
deriving instance Typeable 'AppendHandle
deriving instance Typeable 'AppendMode
deriving instance Typeable 'BlockedIndefinitelyOnMVar
deriving instance Typeable 'BlockedIndefinitelyOnSTM
deriving instance Typeable 'BlockedOnBlackHole
deriving instance Typeable 'BlockedOnException
deriving instance Typeable 'BlockedOnForeignCall
deriving instance Typeable 'BlockedOnMVar
deriving instance Typeable 'BlockedOnOther
deriving instance Typeable 'BlockedOnSTM
deriving instance Typeable 'ClosedHandle
deriving instance Typeable 'ClosePunctuation
deriving instance Typeable 'ConnectorPunctuation
deriving instance Typeable 'Const
deriving instance Typeable 'Control
deriving instance Typeable 'CRLF
deriving instance Typeable 'CurrencySymbol
deriving instance Typeable 'DashPunctuation
deriving instance Typeable 'Deadlock
deriving instance Typeable 'DecimalNumber
deriving instance Typeable 'Denormal
deriving instance Typeable 'Directory
deriving instance Typeable 'DivideByZero
deriving instance Typeable 'Down
deriving instance Typeable 'Dual
deriving instance Typeable 'EnclosingMark
deriving instance Typeable 'Endo
deriving instance Typeable 'Exception.EOF
deriving instance Typeable 'EQ
deriving instance Typeable 'ErrorOnCodingFailure
deriving instance Typeable 'False
deriving instance Typeable 'FinalQuote
deriving instance Typeable 'First
deriving instance Typeable 'ForceSpecConstr
deriving instance Typeable 'Format
deriving instance Typeable 'GT
deriving instance Typeable 'HardwareFault
deriving instance Typeable 'HeapOverflow
deriving instance Typeable 'IgnoreCodingFailure
deriving instance Typeable 'IllegalOperation
deriving instance Typeable 'InappropriateType
deriving instance Typeable 'Data.Infix
deriving instance Typeable 'InitialQuote
deriving instance Typeable 'InputUnderflow
deriving instance Typeable 'Interrupted
deriving instance Typeable 'InvalidArgument
deriving instance Typeable 'InvalidSequence
deriving instance Typeable 'Just
deriving instance Typeable 'K1
deriving instance Typeable 'KProxy
deriving instance Typeable 'Last
deriving instance Typeable 'Left
deriving instance Typeable 'LeftAdjust
deriving instance Typeable 'LeftAssociative
deriving instance Typeable 'LetterNumber
deriving instance Typeable 'LF
deriving instance Typeable 'LineSeparator
deriving instance Typeable 'LossOfPrecision
deriving instance Typeable 'LowercaseLetter
deriving instance Typeable 'LT
deriving instance Typeable 'MaskedInterruptible
deriving instance Typeable 'MaskedUninterruptible
deriving instance Typeable 'MathSymbol
deriving instance Typeable 'ModifierLetter
deriving instance Typeable 'ModifierSymbol
deriving instance Typeable 'NestedAtomically
deriving instance Typeable 'NewlineMode
deriving instance Typeable 'NonSpacingMark
deriving instance Typeable 'NonTermination
deriving instance Typeable 'NoSpecConstr
deriving instance Typeable 'NoSuchThing
deriving instance Typeable 'NotAssigned
deriving instance Typeable 'NotAssociative
deriving instance Typeable 'Nothing
deriving instance Typeable 'O
deriving instance Typeable 'OpenPunctuation
deriving instance Typeable 'OtherError
deriving instance Typeable 'OtherLetter
deriving instance Typeable 'OtherNumber
deriving instance Typeable 'OtherPunctuation
deriving instance Typeable 'OtherSymbol
deriving instance Typeable 'OutputUnderflow
deriving instance Typeable 'Overflow
deriving instance Typeable 'Par1
deriving instance Typeable 'ParagraphSeparator
deriving instance Typeable 'PermissionDenied
deriving instance Typeable 'Data.Prefix
deriving instance Typeable 'PrivateUse
deriving instance Typeable 'Product
deriving instance Typeable 'ProtocolError
deriving instance Typeable 'RatioZeroDenominator
deriving instance Typeable 'RawDevice
deriving instance Typeable 'ReadBuffer
deriving instance Typeable 'ReadHandle
deriving instance Typeable 'ReadMode
deriving instance Typeable 'ReadWriteHandle
deriving instance Typeable 'ReadWriteMode
deriving instance Typeable 'RegularFile
deriving instance Typeable 'RelativeSeek
deriving instance Typeable 'ResourceBusy
deriving instance Typeable 'ResourceExhausted
deriving instance Typeable 'ResourceVanished
deriving instance Typeable 'Right
deriving instance Typeable 'RightAssociative
deriving instance Typeable 'RoundtripFailure
deriving instance Typeable 'SeekFromEnd
deriving instance Typeable 'SemiClosedHandle
deriving instance Typeable 'SignPlus
deriving instance Typeable 'SignSpace
deriving instance Typeable 'Space
deriving instance Typeable 'SpacingCombiningMark
deriving instance Typeable 'SPEC
deriving instance Typeable 'SPEC2
deriving instance Typeable 'StackOverflow
deriving instance Typeable 'Stream
deriving instance Typeable 'Sum
deriving instance Typeable 'Surrogate
deriving instance Typeable 'SystemError
deriving instance Typeable 'ThreadBlocked
deriving instance Typeable 'ThreadDied
deriving instance Typeable 'ThreadFinished
deriving instance Typeable 'ThreadKilled
deriving instance Typeable 'ThreadRunning
deriving instance Typeable 'TimeExpired
deriving instance Typeable 'TitlecaseLetter
deriving instance Typeable 'TransliterateCodingFailure
deriving instance Typeable 'True
deriving instance Typeable 'U1
deriving instance Typeable 'Underflow
deriving instance Typeable 'Unmasked
deriving instance Typeable 'UnsatisfiedConstraints
deriving instance Typeable 'UnsupportedOperation
deriving instance Typeable 'UppercaseLetter
deriving instance Typeable 'UserError
deriving instance Typeable 'UserInterrupt
deriving instance Typeable 'WriteBuffer
deriving instance Typeable 'WriteHandle
deriving instance Typeable 'WriteMode
deriving instance Typeable 'ZeroPad
deriving instance Typeable 'ZipList

#  if defined(mingw32_HOST_OS)
deriving instance Typeable 'Break
deriving instance Typeable 'Catch
deriving instance Typeable 'Close
deriving instance Typeable 'CompactArray
deriving instance Typeable 'ControlC
deriving instance Typeable 'ConvArray
deriving instance Typeable 'Default
deriving instance Typeable 'Ignore
deriving instance Typeable 'Logoff
deriving instance Typeable 'Shutdown
deriving instance Typeable 'SingleByteCP
#   if !defined(__GHCJS__)
deriving instance Typeable 'Break
deriving instance Typeable 'Close
deriving instance Typeable 'ControlC
deriving instance Typeable 'Logoff
deriving instance Typeable 'Shutdown
#   endif
#  endif
# endif

#endif
