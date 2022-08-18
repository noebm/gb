module Utilities.Statistics.IncrementalAverage where

import qualified Data.Vector as V

data IncrementalAverage = IncrementalAverage
  { average :: Double
  , sampleSize :: Word
  }
  deriving (Show)

instance Semigroup IncrementalAverage where
  IncrementalAverage mu1 n1 <> IncrementalAverage mu2 n2 =
    IncrementalAverage mu n
   where
    n = n1 + n2
    mu = (fromIntegral n1 * mu1 + fromIntegral n2 * mu2) / fromIntegral n

instance Monoid IncrementalAverage where
  mempty = IncrementalAverage 0 0

addSample :: Double -> IncrementalAverage -> IncrementalAverage
addSample sample incAvg =
  IncrementalAverage
    { average = mu_prev + (sample - mu_prev) / fromIntegral k
    , sampleSize = k
    }
 where
  k = sampleSize incAvg + 1
  mu_prev = average incAvg
