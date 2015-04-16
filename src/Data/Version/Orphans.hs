{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Version.Orphans () where

#if !MIN_VERSION_base(4,8,0)
import Data.Version

# if MIN_VERSION_base(4,7,0)
import GHC.Exts (IsList(..))
# else
import Data.Data
# endif
#endif

#if !MIN_VERSION_base(4,8,0)
# if MIN_VERSION_base(4,7,0)
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
# else
deriving instance Data Version
# endif
#endif
