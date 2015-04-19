{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

{-# OPTIONS_GHC -fno-warn-deprecations -fno-warn-orphans #-}
module Data.Typeable.Orphans () where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Control.Exception
import Control.Monad.ST.Lazy
import Data.Char
import Data.Data as Data
import Data.Monoid as Monoid
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Pool
import GHC.Conc
import GHC.IO.Buffer
import GHC.IO.Device
import GHC.IO.Encoding
import System.Console.GetOpt
import System.IO
import System.IO.Error
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec
import Text.Read

# if MIN_VERSION_base(4,2,0) && !MIN_VERSION_base(4,3,0)
import Data.Unique
# endif

# if MIN_VERSION_base(4,4,0)
import GHC.Event
import GHC.Fingerprint
import GHC.IO.Encoding.Failure
# endif

# if __GLASGOW_HASKELL__ >= 702
import GHC.Generics as Generics
# endif

# if MIN_VERSION_base(4,5,0)
import GHC.Stack
import GHC.Stats
# endif

#if MIN_VERSION_base(4,6,0)
import Data.Ord
import GHC.TypeLits
# endif

# if MIN_VERSION_base(4,7,0)
import Control.Concurrent.QSem
import Text.Read.Lex
# endif

# if __GLASGOW_HASKELL__ >= 708
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Zip
import Data.Bits
import Data.Fixed
import Data.Foldable
import Data.Ix
import Data.Traversable
import Data.Type.Coercion
import Data.Type.Equality
import Foreign.Storable
import GHC.Exts
import GHC.IO.BufferedIO
import GHC.IP
import Text.Printf
# endif
#endif

#if __GLASGOW_HASKELL__ < 710
deriving instance Typeable  All
deriving instance Typeable1 ArgDescr
deriving instance Typeable1 ArgOrder
deriving instance Typeable  Monoid.Any
deriving instance Typeable  BlockReason
deriving instance Typeable1 Buffer
deriving instance Typeable3 BufferCodec
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
deriving instance Typeable1 Handler
deriving instance Typeable  IODeviceType
deriving instance Typeable  IOErrorType
deriving instance Typeable  IOMode
deriving instance Typeable1 Last
deriving instance Typeable  Lexeme
deriving instance Typeable  Newline
deriving instance Typeable  NewlineMode
deriving instance Typeable1 OptDescr
deriving instance Typeable  Pool
deriving instance Typeable1 Product
deriving instance Typeable1 ReadP
deriving instance Typeable1 ReadPrec
deriving instance Typeable  SeekMode
deriving instance Typeable2 ST
deriving instance Typeable1 Sum
deriving instance Typeable  TextEncoding
deriving instance Typeable  ThreadStatus
deriving instance Typeable1 ZipList

#if MIN_VERSION_base(4,2,0) && !MIN_VERSION_base(4,3,0)
deriving instance Typeable  Unique
# endif

# if MIN_VERSION_base(4,3,0)
deriving instance Typeable  MaskingState
# endif

# if MIN_VERSION_base(4,4,0)
deriving instance Typeable  CodingFailureMode
deriving instance Typeable  CodingProgress
deriving instance Typeable  Event
deriving instance Typeable  EventManager
deriving instance Typeable  FdKey
deriving instance Typeable  Fingerprint
deriving instance Typeable  TimeoutKey
# endif

# if __GLASGOW_HASKELL__ >= 702
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
deriving instance Typeable1 Down
deriving instance Typeable  Nat
deriving instance Typeable  Symbol
# endif

# if MIN_VERSION_base(4,7,0)
deriving instance Typeable FieldFormat
deriving instance Typeable FormatAdjustment
deriving instance Typeable FormatParse
deriving instance Typeable FormatSign
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
# endif

#endif
