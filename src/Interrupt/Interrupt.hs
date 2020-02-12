{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes #-}
module Interrupt.Interrupt
  ( InterruptState (..)
  , interruptVBlank
  , interruptLCD
  , interruptTimer
  , interruptSerial
  , interruptJoypad

  , defaultInterruptState

  , checkForInterrupts
  , interruptAddress
  , interrupt
  , Interrupt

  , inInterruptRange
  , loadInterrupt
  , storeInterrupt
  )
where

import Interrupt.InterruptType

import Control.Monad
import Data.Foldable
import Data.Word
import Data.Bits
import Control.Lens
import Data.Bits.Lens

data InterruptState = InterruptState
  { _interruptVBlank :: InterruptType
  , _interruptLCD    :: InterruptType
  , _interruptTimer  :: InterruptType
  , _interruptSerial :: InterruptType
  , _interruptJoypad :: InterruptType
  } deriving (Show)

makeLenses ''InterruptState

defaultInterruptState :: InterruptState
defaultInterruptState = InterruptState d d d d d
  where d = defaultInterruptType

data Interrupt = INTVBLANK | INTLCD | INTTIMER | INTSERIAL | INTJOYPAD
  deriving (Eq, Enum, Show)

{-# INLINE interruptAddress #-}
interruptAddress :: Interrupt -> Word16
interruptAddress INTVBLANK = 0x40
interruptAddress INTLCD    = 0x48
interruptAddress INTTIMER  = 0x50
interruptAddress INTSERIAL = 0x58
interruptAddress INTJOYPAD = 0x60

{-# INLINE interrupt #-}
interrupt :: Interrupt -> IndexedLens' Interrupt InterruptState InterruptType
interrupt INTVBLANK f = interruptVBlank (indexed f INTVBLANK)
interrupt INTLCD    f = interruptLCD    (indexed f INTLCD)
interrupt INTTIMER  f = interruptTimer  (indexed f INTTIMER)
interrupt INTSERIAL f = interruptSerial (indexed f INTSERIAL)
interrupt INTJOYPAD f = interruptJoypad (indexed f INTJOYPAD)

interrupts :: IndexedTraversal' Interrupt InterruptState InterruptType
interrupts f s
  = InterruptState
    <$> aux INTVBLANK
    <*> aux INTLCD
    <*> aux INTTIMER
    <*> aux INTSERIAL
    <*> aux INTJOYPAD
  where aux i = indexed f i (s ^. interrupt i)

-- find first set interrupt ordered by priority
checkForInterrupts :: InterruptState -> Maybe Interrupt
checkForInterrupts is = findIndexOf interrupts isTriggered is

{-# INLINE interruptBit #-}
interruptBit :: Interrupt -> Int
interruptBit INTVBLANK = 0
interruptBit INTLCD    = 1
interruptBit INTTIMER  = 2
interruptBit INTSERIAL = 3
interruptBit INTJOYPAD = 4

loadInterrupt' l b  = ifoldrOf' (interrupts <. l) (\i x -> bitAt (interruptBit i) .~ x) b
storeInterrupt' l b = interrupts <. l .@~ \i -> b ^. bitAt (interruptBit i)

loadInterrupt :: InterruptState -> Word16 -> Word8
loadInterrupt s 0xff0f = loadInterrupt' interruptFlag    0xe0 s
loadInterrupt s 0xffff = loadInterrupt' interruptEnabled 0x00 s
loadInterrupt _ _ = error "loadInterrupt: not an interrupt address"

storeInterrupt :: InterruptState -> Word16 -> Word8 -> InterruptState
storeInterrupt s0 0xff0f b = storeInterrupt' interruptFlag b s0
storeInterrupt s0 0xffff b = storeInterrupt' interruptEnabled b s0
storeInterrupt _ _ _ = error "storeInterrupt: not an interrupt address"

{-# INLINE inInterruptRange #-}
inInterruptRange :: Word16 -> Bool
inInterruptRange addr = addr == 0xff0f || addr == 0xffff
