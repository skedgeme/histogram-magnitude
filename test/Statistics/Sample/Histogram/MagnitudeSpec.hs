{-# LANGUAGE ScopedTypeVariables #-}
module Statistics.Sample.Histogram.MagnitudeSpec where

import qualified Data.Vector.Unboxed as U
import           Data.Monoid
import           Statistics.Sample.Histogram.Magnitude
import           Test.Hspec 
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

instance Arbitrary Histogram where
  arbitrary = do
    y <- arbitrary
    x <- suchThat arbitrary (\x -> x > -1 && x < 3)
    pure $ mkHistogram (fromIntegral (x :: Int)) (y :: Double)

instance EqProp Histogram where
  x =-= y = eq x y

main :: IO ()
main = hspec spec

spec :: Spec 
spec = describe "Statistics.Sample.Histogram.Magnitude" $ do

  it "calculates a meaningful histogram" $ do
    let x = foldHist 1 [-3, 1, 4, 6, 9]
        result = U.fromList [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1]
    histBuckets x `shouldBe` result

  describe "calculates magnitude" $ do
    -- should be quick check
    it "0" $ histMagnitude (mkHistogram 1 1) `shouldBe` 0
    it "1" $ histMagnitude (mkHistogram 1 19) `shouldBe` 1
    it "2" $ histMagnitude (mkHistogram 1 123) `shouldBe` 2

  it "blends equal resolutions and magnitudes" $ do
    let x = foldHist 1 [1, 4, 6, 9]
        y = foldHist 1 [1, 6]
        result = U.fromList [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 1, 0, 2, 0, 0, 1]
    histBuckets (x <> y) `shouldBe` result

  it "scales non equal magnitudes to the higher magnitude" $ do
    let x = foldHist 1 [1, 4, 6, 9]
        y = foldHist 1 [134, 666]
        result = U.fromList [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 1, 0, 0, 0, 0, 1, 0, 0, 0]
    histMagnitude (x <> y) `shouldBe` 2
    histBuckets (x <> y) `shouldBe` result

  it "blurs non equal resolutions to the lower resolution" $ do
    let x = foldHist 1 [1, 4, 6, 9]
        y = foldHist 2 [1, 6.6]
        result = U.fromList [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 1, 0, 2, 0, 0, 1]
    histResolution (x <> y) `shouldBe` 1
    histBuckets (x <> y) `shouldBe` result

  it "blurs and scales non equal resolution and magnitude" $ do
    let x = foldHist 2 [1, 4, 6, 9]
        y = foldHist 3 [-17, 1, 66]
        vec = (U.replicate 100 0)
        result = U.accum (+) vec [(82, 1)]
              <> U.accum (+) vec [(1, 2), (4, 1), (6, 1), (9, 1), (66, 1)]
    histBuckets (x <> y) `shouldBe` result

  describe "produces keys for given magnitues and scales" $ do
    it "2-1" $
      keys (mkHistogram 2 1) `shouldBe` U.fromList (reverse [0, -0.1 .. -9.9 ] <> [0, 0.1 .. 9.9 :: Double])
    it "2-10" $
      keys (mkHistogram 2 10) `shouldBe` U.fromList ([-99, -98 .. 0] <> [0, 1 .. 99 :: Double])
    it "1-9" $
      keys (mkHistogram 1 9) `shouldBe` U.fromList ([-9, -8 .. 0] <> [0, 1 .. 9 :: Double])

  it "allows insertion" $ do
    let x = foldHist 1 [1, 4, 6, 9]
        result = U.fromList [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1]
    histBuckets (insert (-3) x) `shouldBe` result

  it "obeys monoid laws" $
    quickCheck $ property $ \(x :: Histogram) -> checkBatch stdArgs (monoid x)

  it "is commutable" $
    quickCheck $ property $ isCommut ((<>) :: Histogram -> Histogram -> Histogram)
      

