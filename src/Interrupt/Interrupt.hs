{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes #-}
module Interrupt.Interrupt
  ( InterruptState (..)
  , interruptVBlank
  , interruptLCD
  , interruptTimer
  , interruptSerial
  , interruptJoypad
  , interruptMasterEnableFlag

  , defaultInterruptState

  , handleInterrupt
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

data InterruptState = InterruptState
  { _interruptVBlank :: InterruptType
  , _interruptLCD    :: InterruptType
  , _interruptTimer  :: InterruptType
  , _interruptSerial :: InterruptType
  , _interruptJoypad :: InterruptType
  , _interruptMasterEnableFlag :: Bool
  } deriving (Show)

makeLenses ''InterruptState

defaultInterruptState :: InterruptState
defaultInterruptState = InterruptState d d d d d False
  where d = defaultInterruptType

data Interrupt = INTVBLANK | INTLCD | INTTIMER | INTSERIAL | INTJOYPAD
  deriving (Enum, Show)

{-# INLINE interruptAddress #-}
interruptAddress :: Interrupt -> Word16
interruptAddress INTVBLANK = 0x40
interruptAddress INTLCD    = 0x48
interruptAddress INTTIMER  = 0x50
interruptAddress INTSERIAL = 0x58
interruptAddress INTJOYPAD = 0x60

interrupt :: Interrupt -> IndexedLens' Interrupt InterruptState InterruptType
interrupt INTVBLANK p = interruptVBlank (indexed p INTVBLANK)
interrupt INTLCD    p = interruptLCD    (indexed p INTLCD)
interrupt INTTIMER  p = interruptTimer  (indexed p INTTIMER)
interrupt INTSERIAL p = interruptSerial (indexed p INTSERIAL)
interrupt INTJOYPAD p = interruptJoypad (indexed p INTJOYPAD)

-- find first set interrupt ordered by priority
checkForInterrupts :: InterruptState -> Maybe Interrupt
checkForInterrupts is = find (\i -> is ^. interrupt i . to isTriggered) [ INTVBLANK ..]

handleInterrupt :: InterruptState -> Maybe (Interrupt, InterruptState)
handleInterrupt s = do
  guard (s ^. interruptMasterEnableFlag)
  i <- checkForInterrupts s
  let s' = s & interrupt i %~ clear & interruptMasterEnableFlag .~ False
  return (i , s')

{-# INLINE interruptBit #-}
interruptBit :: Interrupt -> Int
interruptBit INTVBLANK = 0
interruptBit INTLCD    = 1
interruptBit INTTIMER  = 2
interruptBit INTSERIAL = 3
interruptBit INTJOYPAD = 4

setInterruptBit :: Interrupt -> (InterruptType -> Bool) -> InterruptState -> Word8
setInterruptBit i f s = shift (fromIntegral $ fromEnum (f (s ^. interrupt i))) (interruptBit i)

getInterruptState :: Word8 -> (InterruptType -> Bool) -> InterruptState -> Word8
getInterruptState b f s = foldl (\acc i -> acc .|. g i) b [INTVBLANK ..]
  where g i = setInterruptBit i f s

loadInterrupt :: InterruptState -> Word16 -> Word8
loadInterrupt s 0xff0f = getInterruptState 0xe0 _interruptFlag    s
loadInterrupt s 0xffff = getInterruptState 0x00 _interruptEnabled s
loadInterrupt _ _ = error "loadInterrupt: not an interrupt address"

getInterruptBit :: Interrupt
                -> (InterruptType -> Bool -> InterruptType)
                -> (InterruptState -> Word8 -> InterruptState)
getInterruptBit i f s b = s & interrupt i %~ \x -> f x (b `testBit` interruptBit i)

updateInterruptState :: (InterruptType -> Bool -> InterruptType) -> InterruptState -> Word8 -> InterruptState
updateInterruptState f s0 b = foldl
  (\s i -> s & interrupt i %~ \x -> f x (b `testBit` interruptBit i))
  s0 [INTVBLANK ..]

storeInterrupt :: InterruptState -> Word16 -> Word8 -> InterruptState
storeInterrupt s0 0xff0f = updateInterruptState (\it x -> set interruptFlag x it) s0
storeInterrupt s0 0xffff = updateInterruptState (\it x -> set interruptEnabled x it) s0
storeInterrupt _ _ = error "storeInterrupt: not an interrupt address"

{-# INLINE inInterruptRange #-}
inInterruptRange :: Word16 -> Bool
inInterruptRange addr = addr == 0xff0f || addr == 0xffff
