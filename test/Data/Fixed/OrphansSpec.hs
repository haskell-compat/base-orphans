{-# LANGUAGE EmptyDataDecls #-}
module Data.Fixed.OrphansSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck (property)

import Data.Fixed
import Data.Orphans ()

data B7

instance HasResolution B7 where
  resolution _ = 128

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Read (Fixed a) instance" $ do
    -- See https://git.haskell.org/ghc.git/commitdiff/7c38e985aa211ca44039c6d1db9fa13690749c59
    it "should satisfy (read . show = id)" $ do
      property $ \x -> (read (show x) :: Fixed B7) == x
