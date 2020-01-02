{-# LANGUAGE TemplateHaskell #-}
module Interrupt.InterruptType where

import Control.Lens

data InterruptType = InterruptType
  { _interruptEnabled :: Bool
  , _interruptFlag    :: Bool
  } deriving (Show)

makeLenses ''InterruptType

defaultInterruptType :: InterruptType
defaultInterruptType = InterruptType False False

setInterrupt :: InterruptType -> InterruptType
setInterrupt i
  | _interruptEnabled i = i & interruptFlag .~ True
  | otherwise = i

clear :: InterruptType -> InterruptType
clear = interruptFlag .~ False

enable :: InterruptType -> InterruptType
enable = interruptEnabled .~ True

disable :: InterruptType -> InterruptType
disable = interruptEnabled .~ False

isTriggered :: InterruptType -> Bool
isTriggered i = _interruptFlag i && _interruptEnabled i
