{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module System.Posix.Types.OrphansSpec (main, spec) where

import           Control.Applicative (liftA2)

import           Data.Bits (Bits(..))
import           Data.Orphans ()
import           Data.Word

import           System.Posix.Types

import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (NonZero(..))

#include "HsBaseConfig.h"

type HDev = HTYPE_DEV_T

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "CDev" $ do
  describe "Bits instance" $ do
    prop "implements (.&.)" $
      pred2HDevHDev (.&.) (.&.)
    prop "implements (.|.)" $
      pred2HDevHDev (.|.) (.|.)
    prop "implements xor" $
      pred2HDevHDev xor xor
    prop "implements shift" $
      pred2IntHDev shift shift
    prop "implements rotate" $
      pred2IntHDev rotate rotate
    prop "implements setBit" $
      pred2IntHDev setBit setBit
    prop "implements clearBit" $
      pred2IntHDev clearBit clearBit
    prop "implements complementBit" $
      pred2IntHDev complementBit complementBit
    prop "implements testBit" $
      pred2IntEq testBit testBit
    prop "implements complement" $
      pred1HDevHDev complement complement
    prop "implements bit" $
      pred1IntHDev bit bit
    prop "implements bitSize" $
      pred1HDevEq bitSize bitSize
    prop "implements isSigned" $
      pred1HDevEq isSigned isSigned
  describe "Bounded instance" $ do
    it "implements minBound" $
      toInteger (minBound :: CDev) `shouldBe` toInteger (minBound :: HDev)
    it "implements maxBound" $
      toInteger (maxBound :: CDev) `shouldBe` toInteger (maxBound :: HDev)
  describe "Integral instance" $ do
    prop "implements quot" $
      pred2HDevHDev quot quot
    prop "implements rem" $
      pred2HDevHDev rem rem
    prop "implements div" $
      pred2HDevHDev div div
    prop "implements mod" $
      pred2HDevHDev mod mod
    prop "implements quotRem" $
      pred2HDevPair quotRem quotRem
    prop "implements divMod" $
      pred2HDevPair divMod divMod
    prop "implements toInteger" $
      pred1HDevEq toInteger toInteger

eqCDevHDev :: CDev -> HDev -> Bool
eqCDevHDev cDev hDev = toInteger cDev == toInteger hDev

pred1Common :: (b -> c -> d)
            -> (a -> b)
            -> (a -> c)
            -> a -> d
pred1Common = liftA2

pred1HDev :: (b -> c -> Bool)
          -> (CDev -> b)
          -> (HDev -> c)
          -> HDev -> Bool
pred1HDev p f = pred1Common p (f . fromIntegral)

pred1HDevEq :: Eq a => (CDev -> a) -> (HDev -> a) -> HDev -> Bool
pred1HDevEq = pred1HDev (==)

pred1HDevHDev :: (CDev -> CDev) -> (HDev -> HDev) -> HDev -> Bool
pred1HDevHDev = pred1HDev eqCDevHDev

pred1IntHDev :: (Int -> CDev) -> (Int -> HDev) -> Int -> Bool
pred1IntHDev = pred1Common eqCDevHDev

pred2Common :: (c -> d -> e)
            -> (a -> b -> c)
            -> (a -> b -> d)
            -> a -> b -> e
pred2Common p f g x y = p (f x y) (g x y)

pred2HDev :: (a -> b -> Bool)
          -> (CDev -> CDev -> a)
          -> (HDev -> HDev -> b)
          -> NonZero HDev -> NonZero HDev -> Bool
pred2HDev eqv cDevPred hDevPred =
  pred2Common eqv (\nz1 nz2 -> cDevPred (fromIntegral $ getNonZero nz1)
                                        (fromIntegral $ getNonZero nz2))
                  (\nz1 nz2 -> hDevPred (getNonZero nz1)
                                        (getNonZero nz2))

pred2HDevHDev :: (CDev -> CDev -> CDev)
              -> (HDev -> HDev -> HDev)
              -> NonZero HDev -> NonZero HDev -> Bool
pred2HDevHDev = pred2HDev eqCDevHDev

pred2HDevPair :: (CDev -> CDev -> (CDev, CDev))
              -> (HDev -> HDev -> (HDev, HDev))
              -> NonZero HDev -> NonZero HDev -> Bool
pred2HDevPair = pred2HDev $ \(cDev1, cDev2) (hDev1, hDev2) ->
     toInteger cDev1 == toInteger hDev1
  && toInteger cDev2 == toInteger hDev2

pred2Int :: (a -> b -> Bool)
         -> (CDev -> Int -> a)
         -> (HDev -> Int -> b)
         -> HDev -> Int -> Bool
pred2Int eqv cDevPred = pred2Common eqv (cDevPred . fromIntegral)

pred2IntHDev :: (CDev -> Int -> CDev)
             -> (HDev -> Int -> HDev)
             -> HDev -> Int -> Bool
pred2IntHDev = pred2Int eqCDevHDev

pred2IntEq :: Eq a
           => (CDev -> Int -> a)
           -> (HDev -> Int -> a)
           -> HDev -> Int -> Bool
pred2IntEq = pred2Int (==)
