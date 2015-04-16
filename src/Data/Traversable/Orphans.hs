{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Traversable.Orphans () where

#if !MIN_VERSION_base(4,7,0)
import Control.Applicative (Const(..), pure, (<$>))
import Data.Either (Either(..))
import Data.Foldable.Orphans ()
import Data.Function (($))
import Data.Traversable

instance Traversable (Either a) where
    traverse _ (Left x) = pure (Left x)
    traverse f (Right y) = Right <$> f y

instance Traversable ((,) a) where
    traverse f (x, y) = (,) x <$> f y

instance Traversable (Const m) where
    traverse _ (Const m) = pure $ Const m
#endif
