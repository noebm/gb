{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Memory
  ( Memory
  , memoryBootRom
  , accessMemory
  , writeMemory
  )
where

import Control.Monad.State
import Control.Lens

import Data.Word
import Data.Bits
import qualified Data.ByteString as B

import Numeric

data Memory = Memory
  { _cartridge :: B.ByteString
  , _videoRAM :: B.ByteString
  , _internalRAM :: B.ByteString
  , _zeroRAM :: B.ByteString -- includes interrupt register...
  }

makeLenses ''Memory

memory :: B.ByteString -> Memory
memory cart =
  Memory
  { _cartridge = cart
  , _videoRAM = B.replicate 0x2000 0x00
  , _internalRAM = B.replicate 0x2000 0x00
  , _zeroRAM = B.replicate 0x80 0x00
  }

memoryBootRom :: IO Memory
memoryBootRom = do
  let bootStrapName = "DMG_ROM.bin"
  b0 <- B.readFile $ "./" ++ bootStrapName
  let b1 = emptyRom 0x00
  return $ memory $ b0 <> B.drop (B.length b0) b1

accessMemory :: MonadState Memory m => Word16 -> m Word8
accessMemory addr
  -- cartridge access
  | addr < 0x8000 = use (cartridge . singular (ix (fromIntegral addr)))
  -- video ram
  | 0x8000 <= addr && addr < 0xA000 = use (videoRAM    . singular (ix (fromIntegral $ addr .&. 0x1FFF)))
  | 0xA000 <= addr && addr < 0xC000 = error "access to external ram"
  -- internal ram (D000 switchable for cgb)
  | 0xC000 <= addr && addr < 0xE000 = use (internalRAM . singular (ix (fromIntegral $ addr .&. 0x1FFF)))
  -- echo ram
  | 0xE000 <= addr && addr < 0xFE00 = use (internalRAM . singular (ix (fromIntegral $ addr .&. 0x1FFF)))
  | 0xFE00 <= addr && addr < 0xFEA0 = error "access to OAM"
  | 0xFF00 <= addr && addr < 0xFF80 = accessMMIO addr
  | 0xFF80 <= addr {- && addr < 0xFFFF -} = use (zeroRAM     . singular (ix (fromIntegral $ addr .&. 0x7F)))
  | otherwise = error $ "access to " ++ showHex addr ""

writeMemory :: MonadState Memory m => Word16 -> Word8 -> m ()
writeMemory addr
  | addr < 0x8000 = assign (cartridge . singular (ix (fromIntegral addr)))
  | 0x8000 <= addr && addr < 0xA000 = assign (videoRAM . singular (ix (fromIntegral $ addr .&. 0x1FFF)))
  | 0xA000 <= addr && addr < 0xC000 = error "write to external ram"
  | 0xC000 <= addr && addr < 0xE000 = assign (internalRAM . singular (ix (fromIntegral $ addr .&. 0x1FFF)))
  | 0xE000 <= addr && addr < 0xFE00 = assign (internalRAM . singular (ix (fromIntegral $ addr .&. 0x1FFF)))
  | 0xFE00 <= addr && addr < 0xFEA0 = error "write to OAM"
  | 0xFF00 <= addr && addr < 0xFF80 = writeMMIO addr
  | 0xFF80 <= addr {- && addr < 0xFFFF -} = assign (zeroRAM     . singular (ix (fromIntegral $ addr .&. 0x7F)))
  | otherwise = const (error $ "write to " ++ showHex addr "")

emptyRom :: Word8 -> B.ByteString
emptyRom = B.replicate 0x8000

data Timer
data Graphics
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
  , graphics :: Graphics
  , audio :: Audio
  , joypad :: Joypad
  }

accessMMIO :: Monad m => Word16 -> m Word8
accessMMIO addr
  -- no sound
  | 0xFF10 <= addr && addr <= 0xFF26 = return 0
  | otherwise = error "mmio not implemented"

writeMMIO :: Monad m => Word16 -> Word8 -> m ()
writeMMIO addr w = return ()
