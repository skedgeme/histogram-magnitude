{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Statistics.Sample.Histogram.Magnitude
  ( Histogram
  , histResolution
  , histMagnitude
  , histBuckets
  , mkHistogram
  , fromList
  , foldHist
  , keys
  ) where

import qualified Data.Vector.Unboxed as U


fromList :: (RealFrac a, Floating a) => Resolution -> [a] -> Histogram
fromList = foldHist

foldHist :: (RealFrac a, Floating a, Foldable f) => Resolution -> f a -> Histogram
foldHist res = foldMap (mkHistogram res)

newtype Resolution = Resolution Int
  deriving (Show, Eq, Ord, Enum, Bounded, Integral, Real, Num)

newtype Magnitude = Magnitude Int
  deriving (Show, Eq, Ord, Enum, Bounded, Integral, Real, Num)

data Histogram
  = Histogram
  { histResolution :: !Resolution
  , histMagnitude  :: !Magnitude
  , histBuckets    :: !(U.Vector Int) -- vector of: 10 ^ resolution
  } deriving (Show, Eq)

zeroHist :: Histogram
zeroHist = Histogram 0 0 $ U.fromList [1]

keys :: (U.Unbox a, Enum a, RealFrac a, Floating a) => Histogram -> U.Vector a
keys h = U.take size (U.fromList [0, one .. upper])
  where
  size = 10 ^ histResolution h
  one = 10 ** fromIntegral (histMagnitude h - fromIntegral (histResolution h) + 1)
  upper = 10 ** fromIntegral (histMagnitude h + 1)

mkHistogram :: (RealFrac a, Floating a) => Resolution -> a -> Histogram
mkHistogram res v
  | v == 0             = zeroHist
  | bucket == 10 ^ res = Histogram res (mag + 1) $ vec U.// [(0, 1)]
  | otherwise          = Histogram res (mag) $ vec U.// [(bucket, 1)]
  where
  vec    = mkBuckets res
  mag    = magnitude v
  bucket = floor $ v / (10 ** fromIntegral (mag + 1 - fromIntegral res))

mkBuckets :: Resolution -> U.Vector Int
mkBuckets res = U.replicate (10 ^ res) 0 

-- | Commutative Monoid
instance Monoid Histogram where
  mempty = Histogram 0 0 mempty

  mappend !a !b
    | a == mempty   = b
    | b == mempty   = a
    | a == zeroHist = b {histBuckets = U.accum (+) (histBuckets b) [(0, 1)] }
    | b == zeroHist = a {histBuckets = U.accum (+) (histBuckets a) [(0, 1)] }
    | mismatchedRes = smear a b
    | otherwise     = cat
    where
    mismatchedRes = histResolution a /= histResolution b 
    magA = histMagnitude a
    magB = histMagnitude b
    cat
      | magA == magB = a {histBuckets = U.zipWith (+) (histBuckets a) (histBuckets b)}
      | magA < magB  = a `condenseInto` b
      | otherwise    = b `condenseInto` a

condenseInto :: Histogram  -> Histogram -> Histogram
condenseInto !a !b =  scale (histMagnitude b) a `mappend` b

scale :: Magnitude -> Histogram -> Histogram
scale mag h
  | mag == histMagnitude h = h
  | mag > histMagnitude h  =
      h { histBuckets = genericScale
                          mag
                          (mkBuckets $ histResolution h)
                          (histMagnitude h)
                          (histBuckets h)
        , histMagnitude = mag
        }
  | otherwise = error "scale: Cannot scale to a smaller magnitude"

smear :: Histogram -> Histogram -> Histogram
smear !a !b 
  | resA <  resB = a `mappend` blur resA b
  | otherwise    = blur resB a `mappend` b
  where
  resA = histResolution a
  resB = histResolution b

blur :: Resolution -> Histogram -> Histogram
blur res h 
  | res == histResolution h = h
  | res < histResolution h  =
      h { histResolution = res
        , histBuckets = genericScale
                          (histResolution h)
                          (mkBuckets res)
                          res
                          (histBuckets h)
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
