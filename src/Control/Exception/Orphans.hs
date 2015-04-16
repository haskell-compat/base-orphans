{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Exception.Orphans () where

#if !MIN_VERSION_base(4,7,0)
import Control.Exception

deriving instance Ord ErrorCall
deriving instance Eq ErrorCall
#endif
