module Instruction.Time where

data Time a
  = ConstantTime a
  | VariableTime { minTime :: a , maxTime :: a }
  deriving Show

getTime :: Bool -> Time a -> a
getTime _longPath (ConstantTime x) = x
getTime longPath (VariableTime short long) = if longPath then long else short
