{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Statistics.Sample.Histogram.Magnitude
  ( Histogram
  , fromList
  , foldHist
  , histBuckets
  , insert
  , keys
  , positiveKeys
  , negativeKeys
  , histPositive
  , histNegative
  , histResolution
  , histMagnitude
  ) where

import qualified Data.Vector.Unboxed as U
import           Data.Monoid

-- | Create a histogram from a list
fromList :: (RealFrac a, Floating a) => Resolution -> [a] -> Histogram
fromList = foldHist

-- | Create a histogram from any Foldable
foldHist :: (RealFrac a, Floating a, Foldable f) => Resolution -> f a -> Histogram
foldHist res = foldMap (mkHistogram res)

-- | Insert a value into a histogram
insert :: (RealFrac a, Floating a) => a -> Histogram -> Histogram
insert v h = mkHistogram (histResolution h) v <> h

newtype Resolution = Resolution Int
  deriving (Show, Eq, Ord, Enum, Bounded, Integral, Real, Num)

newtype Magnitude = Magnitude Int
  deriving (Show, Eq, Ord, Enum, Bounded, Integral, Real, Num)

data Histogram
  = Histogram
  { histResolution :: !Resolution
  , histMagnitude  :: !Magnitude
  , histPositive   :: !(U.Vector Int) -- vector of: 10 ^ resolution
  , histNegative   :: !(U.Vector Int) -- vector of: 10 ^ resolution
  } deriving (Show, Eq)

histBuckets :: Histogram -> U.Vector Int
histBuckets h = U.reverse (histNegative h) <> histPositive h

zeroHist :: Histogram
zeroHist = Histogram 0 0 (U.fromList [1]) mempty

keys :: (U.Unbox a, Enum a, RealFrac a, Floating a) => Histogram -> U.Vector a
keys h = U.reverse (negativeKeys h) <> positiveKeys h

negativeKeys :: (U.Unbox a, Enum a, RealFrac a, Floating a) => Histogram -> U.Vector a
negativeKeys h = U.map negate $ positiveKeys h

positiveKeys :: (U.Unbox a, Enum a, RealFrac a, Floating a) => Histogram -> U.Vector a
positiveKeys h = U.take size (U.fromList [0, one .. upper])
  where
  size = 10 ^ histResolution h
  one = 10 ** fromIntegral (histMagnitude h - fromIntegral (histResolution h) + 1)
  upper = 10 ** fromIntegral (histMagnitude h + 1)

mkHistogram :: (RealFrac a, Floating a) => Resolution -> a -> Histogram
mkHistogram res v
  | v == 0           = zeroHist
  | v <  0           = Histogram res (mag) vec (vec U.// [(bucket, 1)])
  | v < 0 && nextMag = Histogram res (mag + 1) vec (vec U.// [(0, 1)])
  | nextMag          = Histogram res (mag + 1) (vec U.// [(0, 1)]) vec
  | otherwise        = Histogram res (mag) (vec U.// [(bucket, 1)]) vec
  where
  vec    = mkBuckets res
  mag    = magnitude (abs v)
  bucket = calcBucket res mag (abs v)
  nextMag = bucket == bucketCeiling res

mkBuckets :: Resolution -> U.Vector Int
mkBuckets res = U.replicate (10 ^ res) 0 

-- | Commutative Monoid
instance Monoid Histogram where
  mempty = Histogram 0 0 mempty mempty

  mappend !a !b
    | a == mempty   = b
    | b == mempty   = a
    | a == zeroHist = b {histPositive = U.accum (+) (histPositive b) [(0, 1)] }
    | b == zeroHist = a {histPositive = U.accum (+) (histPositive a) [(0, 1)] }
    | mismatchedRes = smear a b
    | otherwise     = cat
    where
    mismatchedRes = histResolution a /= histResolution b 
    magA = histMagnitude a
    magB = histMagnitude b
    cat
      | magA == magB = a { histPositive = U.zipWith (+) (histPositive a) (histPositive b)
                         , histNegative = U.zipWith (+) (histNegative a) (histNegative b)
                         }
      | magA < magB  = a `condenseInto` b
      | otherwise    = b `condenseInto` a

condenseInto :: Histogram  -> Histogram -> Histogram
condenseInto !a !b =  scale (histMagnitude b) a <> b

scale :: Magnitude -> Histogram -> Histogram
scale mag h
  | mag == histMagnitude h = h
  | mag > histMagnitude h  =
      h { histPositive = genericScale
                            mag
                            (mkBuckets $ histResolution h)
                            (histMagnitude h)
                            (histPositive h)
        , histNegative = genericScale
                            mag
                            (mkBuckets $ histResolution h)
                            (histMagnitude h)
                            (histNegative h)
        , histMagnitude = mag
        }
  | otherwise = error "scale: Cannot scale to a smaller magnitude"

smear :: Histogram -> Histogram -> Histogram
smear !a !b 
  | resA <  resB = a <> blur resA b
  | otherwise    = blur resB a <> b
  where
  resA = histResolution a
  resB = histResolution b

blur :: Resolution -> Histogram -> Histogram
blur res h 
  | res == histResolution h = h
  | res < histResolution h  =
      h { histResolution = res
        , histPositive = genericScale
                            (histResolution h)
                            (mkBuckets res)
                            res
                            (histPositive h)
        , histNegative = genericScale
                            (histResolution h)
                            (mkBuckets res)
                            res
                            (histNegative h)
        }
  | otherwise = error "blur: Cannot blur to a greater resolution"

genericScale :: Integral a => a -> U.Vector Int -> a -> U.Vector Int ->  U.Vector Int
genericScale to defBuckets from buckets = U.accum (+) defBuckets accums
  where
  divisor       = 10 ^ (to - from) :: Double
  scale' (x, y) = (floor (x / divisor), y)
  accums        = fmap scale' . zip [0..] $ U.toList buckets

magnitude :: (RealFrac a, Floating a) => a -> Magnitude
magnitude n = floor $ logBase 10 n

calcBucket :: (RealFrac a, Floating a) => Resolution -> Magnitude -> a -> Int
calcBucket res mag v = floor $ v / (10 ** fromIntegral (mag + 1 - fromIntegral res))

bucketCeiling :: Resolution -> Int
bucketCeiling res = 10 ^ res
