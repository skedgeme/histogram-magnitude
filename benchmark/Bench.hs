module Main where

import Criterion.Main
import Statistics.Sample.Histogram.Magnitude
import Data.Functor.Identity

main = defaultMain
  [ bgroup "resolution" [ bench "1"  . nf (foldHist 1) $ Identity (1 :: Double)
                        , bench "2"  . nf (foldHist 2) $ Identity (1 :: Double)
                        , bench "3"  . nf (foldHist 3) $ Identity (1 :: Double)
                        ]
  , bgroup "size"       [ bench "0"      $ nf (foldHist 2) ([] :: [Double])
                        , bench "0..9"   $ nf (foldHist 2) [0..9 :: Double]
                        , bench "0..99"  $ nf (foldHist 2) [0..99 :: Double]
                        ]
  ]
