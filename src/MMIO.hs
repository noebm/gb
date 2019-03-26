{-# LANGUAGE FlexibleContexts #-}
module MMIO
  ( writeMMIO
  , accessMMIO
  , MMIO
  , defaultMMIO
  , canAccessVRAM
  )
where

import Data.Word
import Data.Bits

import Control.Monad.State

data Timer
data Audio
data Joypad

{-
data SweepDirection = SweepUp | SweepDown

data Sweep = Sweep
  { sweepTime :: Word8 -- ^ sweep time in k / 128 Hz with k in range [7..0]
  , sweepDirection :: SweepDirection
  , sweepShiftNumber :: Word8 -- ^ scaling number in range [7..0]
  }

data SoundChannel1 = SoundChannel1
  { sweep :: Maybe Sweep
  }

getSweep :: Word8 -> Maybe Sweep
getSweep w =
  if sweept == 0x00
  then Nothing
  else Just $ Sweep
       { sweepTime = sweept
       , sweepDirection = if w `testBit` 3 then SweepDown else SweepUp
       , sweepShiftNumber = w .&. 0x07
       }
  where sweept = (w `shiftR` 4) .&. 0x07

writeSweep :: Maybe Sweep -> Word8
writeSweep Nothing = 0x00
writeSweep (Just x)
  =   (sweepTime x `shiftL` 4)
  .|. (case sweepDirection x of SweepDown -> 0x08 ; SweepUp -> 0x00)
  .|. sweepShiftNumber x

applySweep :: Fractional a => Sweep -> a -> a
applySweep s x
  | SweepUp <- sweepDirection s = x * (1 + factor)
  | otherwise                   = x * (1 - factor)
  where factor = 1 / 2^ sweepShiftNumber s
-}

data MMIO = MMIO
  { timer :: Timer
  , display :: Display
  , audio :: Audio
  , joypad :: Joypad
  }

defaultMMIO :: MMIO
defaultMMIO = MMIO {}

accessMMIO :: MonadState MMIO m => Word16 -> m Word8
accessMMIO addr
  -- no sound
  | 0xFF10 <= addr && addr <= 0xFF26 = return 0
  | otherwise = error "mmio not implemented"

writeMMIO :: MonadState MMIO m => Word16 -> Word8 -> m ()
writeMMIO addr w = return ()

data Display = Display
  { controlRegister :: Word8 -- 0xFF40
  , statusRegister :: Word8 -- 0xFF41
  , scrollX :: Word8 -- 0xFF42
  , scrollY :: Word8 -- 0xFF43
  , yCoord :: Word8 -- 0xFF44
  , yCompare :: Word8 -- 0xFF45
  }

canAccessVRAM :: MMIO -> Bool
canAccessVRAM dpy = True

-- accessDisplay


