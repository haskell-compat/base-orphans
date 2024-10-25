module GHC.Fingerprint.OrphansSpec (main, spec) where

import Test.Hspec

import Data.Orphans ()
import Data.Word (Word64)
import GHC.Fingerprint.Type

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Fingerprint" $
    it "has a Show instance" $ do
      let w1, w2 :: Word64
          w1 = 0x0123456789abcdef
          w2 = 0x42

          f :: Fingerprint
          f = Fingerprint w1 w2
      show f `shouldBe` "0123456789abcdef0000000000000042"
