{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Hardware.Interrupt
  ( Interrupt (..)

  , InterruptState
  , defaultInterruptState

  , interruptEnable
  , interruptFlag

  , interruptAddress

  , storeIntFlag, loadIntFlag
  , storeIntEnable, loadIntEnable

  , checkForInterrupts
  )
where

import Control.Lens
import Data.Bits.Lens
import Data.Bits
import Data.Word

data InterruptState = InterruptState
  { _interruptEnable' :: {-# UNPACK #-} !Word8
  , _interruptFlag'   :: {-# UNPACK #-} !Word8
  } deriving Show

makeLenses ''InterruptState

defaultInterruptState :: InterruptState
defaultInterruptState = InterruptState 0xe0 0xe0

data Interrupt = INTVBLANK | INTLCD | INTTIMER | INTSERIAL | INTJOYPAD
  deriving (Eq, Enum, Show)

{-# INLINE interruptAddress #-}
interruptAddress :: Interrupt -> Word16
interruptAddress INTVBLANK = 0x40
interruptAddress INTLCD    = 0x48
interruptAddress INTTIMER  = 0x50
interruptAddress INTSERIAL = 0x58
interruptAddress INTJOYPAD = 0x60

{-# INLINE interrupt' #-}
interrupt' :: Interrupt -> Lens' Word8 Bool
interrupt' int = bitAt (fromEnum int)

interrupts' :: IndexedTraversal' Interrupt Word8 Bool
interrupts' f s = foldr step 0xe0 <$> traverse g bs where
  g int = (,) int <$> indexed f int (s ^. interrupt' int)
  bs = [INTVBLANK .. INTJOYPAD]
  step (int, True) r = r & interrupt' int .~ True
  step _ r = r

checkForInterrupts :: InterruptState -> Maybe Interrupt
checkForInterrupts (InterruptState inte intf)
  = findIndexOf interrupts' id (inte .&. intf)

interruptEnable :: Interrupt -> Lens' InterruptState Bool
interruptEnable int = interruptEnable' . interrupt' int

interruptFlag :: Interrupt -> Lens' InterruptState Bool
interruptFlag int = interruptFlag' . interrupt' int

storeIntEnable :: Word8 -> InterruptState -> InterruptState
storeIntEnable byte s = s & interruptEnable' .~ (0xe0 .|. (0x1f .&. byte))

storeIntFlag :: Word8 -> InterruptState -> InterruptState
storeIntFlag byte s = s & interruptFlag' .~ (0xe0 .|. (0x1f .&. byte))

loadIntEnable :: InterruptState -> Word8
loadIntEnable = view interruptEnable'

loadIntFlag :: InterruptState -> Word8
loadIntFlag = view interruptFlag'
