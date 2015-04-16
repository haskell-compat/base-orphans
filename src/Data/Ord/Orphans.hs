{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Ord.Orphans () where

#if MIN_VERSION_base(4,6,0) && !MIN_VERSION_base(4,7,0)
import Data.Ord

deriving instance Read a => Read (Down a)
deriving instance Show a => Show (Down a)
#endif
