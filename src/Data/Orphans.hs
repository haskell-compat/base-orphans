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

#if !(MIN_VERSION_base(4,4,0))
import           Control.Monad.ST as Strict
#endif

#if __GLASGOW_HASKELL__ >= 701 && __GLASGOW_HASKELL__ < 710
import           GHC.Generics as Generics
#endif

#if __GLASGOW_HASKELL__ < 710
import           Control.Exception as Exception
import           Control.Monad.ST.Lazy as Lazy
import           Data.Data as Data
import qualified Data.Foldable as F (Foldable(..))
import           Data.Monoid as Monoid
import qualified Data.Traversable as T (Traversable(..))
import           GHC.Exts as Exts
import           GHC.IO.Exception as Exception
import           Text.ParserCombinators.ReadP as ReadP
import           Text.ParserCombinators.ReadPrec as ReadPrec
import           Text.Read as Read

# if defined(mingw32_HOST_OS)
import           GHC.ConsoleHandler as Console
# endif
#endif

#if !(MIN_VERSION_base(4,8,0))
import           Data.Orphans.Prelude
#endif

#include "HsBaseConfig.h"

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
#endif

#if !(MIN_VERSION_base(4,6,0))
# if defined(HTYPE_DEV_T)
#  if MIN_VERSION_base(4,5,0)
deriving instance Bits CDev
deriving instance Bounded CDev
deriving instance Integral CDev
#  else
type HDev = HTYPE_DEV_T

instance Bits CDev where
    (.&.)         = unsafeCoerce ((.&.)         :: HDev -> HDev -> HDev)
    (.|.)         = unsafeCoerce ((.|.)         :: HDev -> HDev -> HDev)
    xor           = unsafeCoerce (xor           :: HDev -> HDev -> HDev)
    shift         = unsafeCoerce (shift         :: HDev -> Int  -> HDev)
    rotate        = unsafeCoerce (rotate        :: HDev -> Int  -> HDev)
    setBit        = unsafeCoerce (setBit        :: HDev -> Int  -> HDev)
    clearBit      = unsafeCoerce (clearBit      :: HDev -> Int  -> HDev)
    complementBit = unsafeCoerce (complementBit :: HDev -> Int  -> HDev)
    testBit       = unsafeCoerce (testBit       :: HDev -> Int  -> Bool)
    complement    = unsafeCoerce (complement    :: HDev -> HDev)
    bit           = unsafeCoerce (bit           :: Int  -> HDev)
    bitSize       = unsafeCoerce (bitSize       :: HDev -> Int)
    isSigned      = unsafeCoerce (isSigned      :: HDev -> Bool)

instance Bounded CDev where
    minBound = unsafeCoerce (minBound :: HDev)
    maxBound = unsafeCoerce (maxBound :: HDev)

instance Integral CDev where
    quot      = unsafeCoerce (quot      :: HDev -> HDev -> HDev)
    rem       = unsafeCoerce (rem       :: HDev -> HDev -> HDev)
    div       = unsafeCoerce (div       :: HDev -> HDev -> HDev)
    mod       = unsafeCoerce (mod       :: HDev -> HDev -> HDev)
    quotRem   = unsafeCoerce (quotRem   :: HDev -> HDev -> (HDev, HDev))
    divMod    = unsafeCoerce (divMod    :: HDev -> HDev -> (HDev, HDev))
    toInteger = unsafeCoerce (toInteger :: HDev -> Integer)
#  endif
# endif

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

instance Functor Exception.Handler where
     fmap f (Exception.Handler h) = Exception.Handler (fmap f . h)

instance
# if MIN_VERSION_base(4,4,0)
  Arrow a
# else
  ArrowApply a
# endif
  => Functor (ArrowMonad a) where
    fmap f (ArrowMonad m) = ArrowMonad $ m >>> arr f

instance
# if MIN_VERSION_base(4,4,0)
  Arrow a
# else
  ArrowApply a
# endif
  => Applicative (ArrowMonad a) where
   pure x = ArrowMonad (arr (const x))
   ArrowMonad f <*> ArrowMonad x = ArrowMonad (f &&& x >>> arr (uncurry id))

instance
# if MIN_VERSION_base(4,4,0)
  ArrowPlus a
# else
  (ArrowApply a, ArrowPlus a)
# endif
  => Alternative (ArrowMonad a) where
   empty = ArrowMonad zeroArrow
   ArrowMonad x <|> ArrowMonad y = ArrowMonad (x <+> y)

instance (ArrowApply a, ArrowPlus a) => MonadPlus (ArrowMonad a) where
   mzero = ArrowMonad zeroArrow
   ArrowMonad x `mplus` ArrowMonad y = ArrowMonad (x <+> y)
#endif

#if !(MIN_VERSION_base(4,7,0))
deriving instance F.Foldable (Const m)
deriving instance F.Foldable (Either a)
deriving instance T.Traversable (Const m)
deriving instance T.Traversable (Either a)

instance F.Foldable ((,) a) where
    foldMap f (_, y) = f y

    foldr f z (_, y) = f y z

instance T.Traversable ((,) a) where
    traverse f (x, y) = (,) x <$> f y

deriving instance Monoid a => Monoid (Const a b)
deriving instance Read a => Read (Down a)
deriving instance Show a => Show (Down a)
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
deriving instance Typeable F.Foldable
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
deriving instance Typeable T.Traversable
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
deriving instance Typeable 'Close
deriving instance Typeable 'ControlC
deriving instance Typeable 'Logoff
deriving instance Typeable 'Shutdown
#  endif
# endif

#endif
