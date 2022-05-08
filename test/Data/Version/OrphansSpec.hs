{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,7,0)
{-# LANGUAGE OverloadedLists #-}
#endif
module Data.Version.OrphansSpec (main, spec) where

import Test.Hspec

import Data.Data
import Data.Orphans ()
import Data.Version

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Data Version instance" $
    it "allows obtaining a Version constructor" $
      dataTypeName (dataTypeOf (Version [1,2,3] [])) `shouldBe`
-- Some old versions of GHC incorrectly return "Version" instead of
-- "Data.Version.Version" due to
-- https://gitlab.haskell.org/ghc/ghc/-/issues/20371.
#if __GLASGOW_HASKELL__ >= 801 && __GLASGOW_HASKELL__ < 903
        "Version"
#else
        "Data.Version.Version"
#endif

#if MIN_VERSION_base(4,7,0)
  describe "IsList Version instance" $
    it "creates a Version from an Int list" $
      [1,2,3] `shouldBe` Version [1,2,3] []
#endif
