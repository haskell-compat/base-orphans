{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Monoid.Orphans () where

#if !MIN_VERSION_base(4,7,0)
import Data.Monoid

deriving instance Num a => Num (Sum a)
deriving instance Num a => Num (Product a)
#endif
