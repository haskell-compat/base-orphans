{-|
Exports orphan instances that mimic instances available in later versions of @base@.
To use them, simply @import Data.Orphans ()@.
-}
module Data.Orphans () where

import Control.Applicative.Orphans   ()
import Control.Exception.Orphans     ()

import Data.Bits.Orphans             ()
import Data.Foldable.Orphans         ()
import Data.Monoid.Orphans           ()
import Data.Ord.Orphans              ()
import Data.Traversable.Orphans      ()
import Data.Typeable.Orphans         ()
import Data.Version.Orphans          ()

import Foreign.Storable.Orphans      ()

import GHC.Generics.Orphans          ()

import System.Console.GetOpt.Orphans ()
