module Statistics.Sample.Histogram.MagnitudeSpec where

import Statistics.Sample.Histogram.Magnitude
import Test.Hspec 
import qualified Data.Vector.Unboxed as U
import Data.Monoid

main :: IO ()
main = hspec spec

spec :: Spec 
spec = describe "Statistics.Sample.Histogram.Magnitude" $ do

  it "calculates a meaningful histogram" $ do
    let x = foldHist 1 [1, 4, 6, 9]
    histBuckets x `shouldBe` U.fromList [0, 1, 0, 0, 1, 0, 1, 0, 0, 1]

  describe "calculates magnitude" $ do
    -- should be quick check
    it "0" $ histMagnitude (mkHistogram 1 1) `shouldBe` 0
    it "1" $ histMagnitude (mkHistogram 1 19) `shouldBe` 1
    it "2" $ histMagnitude (mkHistogram 1 123) `shouldBe` 2

  it "blends equal resolutions and magnitudes" $ do
    let x = foldHist 1 [1, 4, 6, 9]
        y = foldHist 1 [1, 6]
    histBuckets (x <> y) `shouldBe` U.fromList [0, 2, 0, 0, 1, 0, 2, 0, 0, 1]

  it "scales non equal magnitudes" $ do
    let x = foldHist 1 [1, 4, 6, 9]
        y = foldHist 1 [134, 666]
    histBuckets (x <> y) `shouldBe` U.fromList [4, 1, 0, 0, 0, 0, 1, 0, 0, 0]

  it "blurs non equal resolutions" $ do
    let x = foldHist 1 [1, 4, 6, 9]
        y = foldHist 2 [1, 6.6]
    histBuckets (x <> y) `shouldBe` U.fromList [0, 2, 0, 0, 1, 0, 2, 0, 0, 1]

  it "blurs and scales non equal resolution and magnitude" $ do
    let x = foldHist 2 [1, 4, 6, 9]
        y = foldHist 3 [1, 66]
    histBuckets (x <> y) `shouldBe` U.accum (+) (U.replicate 100 0) [(1, 2), (4, 1), (6, 1), (9, 1), (66, 1)] 

  describe "produces keys for given magnitues and scales" $ do
    it "2-1" $ do
      pendingWith "not numerically stable."
      keys (mkHistogram 2 1) `shouldBe` U.fromList [0, 0.1 .. 9 :: Double]
    it "2-10" $ keys (mkHistogram 2 10) `shouldBe` U.fromList [0, 1 .. 99 :: Double]
    it "1-9" $ keys (mkHistogram 1 9) `shouldBe` U.fromList [0 .. 9 :: Double]

