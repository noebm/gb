module Interrupt.InterruptType where

data InterruptType = InterruptType
  { interruptEnabled :: Bool
  , interruptFlag    :: Bool
  }

defaultInterruptType :: InterruptType
defaultInterruptType = InterruptType False False

set :: InterruptType -> InterruptType
set i
  | interruptEnabled i = i { interruptFlag = True }
  | otherwise = i

clear :: InterruptType -> InterruptType
clear i = i { interruptFlag = False }

enable :: InterruptType -> InterruptType
enable i = i { interruptEnabled = True }

disable :: InterruptType -> InterruptType
disable i = i { interruptEnabled = False }

isTriggered :: InterruptType -> Bool
isTriggered i = interruptFlag i && interruptEnabled i
