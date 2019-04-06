module Interrupt.Interrupt
  ( InterruptState (..)
  , defaultInterruptState

  , handleInterrupt
  , interruptAddress

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

data InterruptState = InterruptState
  { interruptVBlank :: InterruptType
  , interruptLCD    :: InterruptType
  , interruptTimer  :: InterruptType
  , interruptSerial :: InterruptType
  , interruptJoypad :: InterruptType
  , interruptStateEnabled :: Bool
  } deriving (Show)

defaultInterruptState :: InterruptState
defaultInterruptState = InterruptState d d d d d False
  where d = defaultInterruptType

disableInterruptState :: InterruptState -> InterruptState
disableInterruptState s = s { interruptStateEnabled = False }

data Interrupt = INTVBLANK | INTLCD | INTTIMER | INTSERIAL | INTJOYPAD
  deriving (Enum, Show)

{-# INLINE getInterrupt #-}
getInterrupt :: Interrupt -> InterruptState -> InterruptType
getInterrupt INTVBLANK = interruptVBlank
getInterrupt INTLCD    = interruptLCD
getInterrupt INTTIMER  = interruptTimer
getInterrupt INTSERIAL = interruptSerial
getInterrupt INTJOYPAD = interruptJoypad

setInterrupt :: Interrupt -> (InterruptType -> InterruptType) -> (InterruptState -> InterruptState)
setInterrupt INTVBLANK f s = s { interruptVBlank = f (interruptVBlank s) }
setInterrupt INTLCD    f s = s { interruptLCD    = f (interruptLCD    s) }
setInterrupt INTTIMER  f s = s { interruptTimer  = f (interruptTimer  s) }
setInterrupt INTSERIAL f s = s { interruptSerial = f (interruptSerial s) }
setInterrupt INTJOYPAD f s = s { interruptJoypad = f (interruptJoypad s) }

{-# INLINE interruptAddress #-}
interruptAddress :: Interrupt -> Word16
interruptAddress INTVBLANK = 0x40
interruptAddress INTLCD    = 0x48
interruptAddress INTTIMER  = 0x50
interruptAddress INTSERIAL = 0x58
interruptAddress INTJOYPAD = 0x60

checkForInterrupts :: InterruptState -> Maybe Interrupt
checkForInterrupts is
  = find (\i -> isTriggered (getInterrupt i is))
  [ INTVBLANK ..]

handleInterrupt :: InterruptState -> Maybe (Interrupt, InterruptState)
handleInterrupt s = do
  guard (interruptStateEnabled s)
  i <- checkForInterrupts s
  let s' = disableInterruptState $ setInterrupt i clear s
  return (i , s')

{-# INLINE interruptBit #-}
interruptBit :: Interrupt -> Int
interruptBit INTVBLANK = 0
interruptBit INTLCD    = 1
interruptBit INTTIMER  = 2
interruptBit INTSERIAL = 3
interruptBit INTJOYPAD = 4

setInterruptBit :: Interrupt -> (InterruptType -> Bool) -> InterruptType -> Word8
setInterruptBit i f s = if f s then bit (interruptBit i) else zeroBits

getInterruptState :: Word8 -> (InterruptType -> Bool) -> InterruptState -> Word8
getInterruptState b f s = foldl (\acc i -> acc .|. g i) b [INTVBLANK ..]
  where g i = setInterruptBit i f (getInterrupt i s)

loadInterrupt :: InterruptState -> Word16 -> Word8
loadInterrupt s 0xff0f = getInterruptState 0xe0 interruptFlag    s
loadInterrupt s 0xffff = getInterruptState 0x00 interruptEnabled s
loadInterrupt _ _ = error "loadInterrupt: not an interrupt address"

getInterruptBit :: Interrupt
                -> (InterruptType -> Bool -> InterruptType)
                -> (InterruptState -> Word8 -> InterruptState)
getInterruptBit i f s b = setInterrupt i g s
  where g x = f x (b `testBit` interruptBit i)

updateInterruptState :: (InterruptType -> Bool -> InterruptType) -> InterruptState -> Word8 -> InterruptState
updateInterruptState f s0 b = foldl (\s i -> getInterruptBit i f s b) s0 [INTVBLANK ..]

storeInterrupt :: InterruptState -> Word16 -> Word8 -> InterruptState
storeInterrupt s0 0xff0f b = updateInterruptState f s0 b
  where f it x = it { interruptFlag = x }
storeInterrupt s0 0xffff b = updateInterruptState f s0 b
  where f it x = it { interruptEnabled = x }
storeInterrupt _ _ _ = error "storeInterrupt: not an interrupt address"

{-# INLINE inInterruptRange #-}
inInterruptRange :: Word16 -> Bool
inInterruptRange addr = addr == 0xff0f || addr == 0xffff
