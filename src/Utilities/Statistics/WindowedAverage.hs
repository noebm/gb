module Utilities.Statistics.WindowedAverage where

import Utilities.Statistics.CircularBuffer

data WindowedAverage a = WindowedAverage
  { window :: CircularBuffer a
  , averageWin :: a
  }

-- will be incorrect as long a the buffer is not filled
emptyWindow :: Int -> a -> WindowedAverage a
emptyWindow size def = WindowedAverage
  { window = fromList $ replicate size def
  , averageWin = def
  }

addWindowSample :: Fractional a => a -> WindowedAverage a -> WindowedAverage a
addWindowSample sample win = win
  { window = window'
  , averageWin = averageWin win + (sample - old) / fromIntegral (lengthCirc (window win))
  }
  where (old, window') = writeCirc sample (window win)
