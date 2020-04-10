module Utilities.Statistics.CircularBuffer where

import qualified Data.Vector as V

data CircularBuffer a = CircularBuffer
  { buffer :: V.Vector a
  , focus :: Int -- oldest element
  }

fromList :: [ a ] -> CircularBuffer a
fromList xs = CircularBuffer (V.fromList xs) 0

writeCirc :: a -> CircularBuffer a -> (a , CircularBuffer a)
writeCirc x b = (buffer b V.! focus b , b { buffer = buffer b V.// [ (focus b, x) ] , focus = newFocus })
  where newFocus = (focus b + 1) `rem` V.length (buffer b)

lengthCirc :: CircularBuffer a -> Int
lengthCirc circ = V.length (buffer circ)
