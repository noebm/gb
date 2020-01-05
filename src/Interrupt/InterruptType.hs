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

isTriggered :: InterruptType -> Bool
isTriggered i = _interruptFlag i && _interruptEnabled i
